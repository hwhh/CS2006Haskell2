import Network
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Fix (fix)
import Control.Exception
import Board

import AI
import NetworkStuff
import Draw
import Input

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Data.List.Split

import Debug.Trace

mainLoop :: Handle -> IO ()
mainLoop handler = do
	fstLine <- hGetLine handler
	if fstLine == "player 1"
		then putStrLn "Connected as Player 1\nWaiting for second player..." >> do l <- hGetLine handler; putStrLn l >> runGame handler Black
		else putStrLn "Connected as Player 2" >> runGame handler White

runGame :: Handle -> Col -> IO ()
runGame handler col = do
--	chan <- newChan
	pictures <- getBoard

	initWorld <- makeWorld (Flags { hardAi = False, whiteP = (col == White) } )
	playIO (InWindow "Gomoku" (740, 580) (10, 10)) white 10
		initWorld
		(drawWorld pictures)
		handleInput
		(advance col handler)


advance :: Col -> Handle -> Float -> World -> IO World
advance col handler t w = if turn w == col then let str = createStr w in do hPutStrLn handler (trace str str) >> return w
			  else do ls <- hGetLine handler; return $ trace (show (splitOn "**" ls)) (getWorld (splitOn "**" ls) w)
main :: IO ()
main = do
	handler <- connectTo "127.0.0.1" (PortNumber 8080)
	mainLoop handler
