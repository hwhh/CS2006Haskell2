module Main where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (liftM)
import Control.Monad.Fix (fix)
import Control.Exception

import Debug.Trace

main :: IO()
main = do
	sock <- socket AF_INET Stream 0
	setSocketOption sock ReuseAddr 1
	bind sock (SockAddrInet 8080 iNADDR_ANY)
	listen sock 2
--	chan <- newChan
--	mainLoop sock chan
	mainLoop3 sock

mainLoop3 :: Socket -> IO()
mainLoop3 sock = do
	let endMsg handler = hPutStrLn handler "\nsession ended"

	player1 <- accept sock
	handler1 <- socketToHandle (fst player1) ReadWriteMode
	hSetBuffering handler1 NoBuffering
	hPutStrLn handler1 "player 1"

	player2 <- accept sock
	handler2 <- socketToHandle (fst player2) ReadWriteMode
	hSetBuffering handler2 NoBuffering
	hPutStrLn handler1 "Player 2 connected"
	hPutStrLn handler2 "player 2"

	chan <- newChan
	reader <- forkIO (fix $ \loop -> do
		msg <- liftM init (hGetLine handler1)
		hPutStrLn handler2 (trace (show msg) msg)

		writeChan chan msg

		msg2 <- liftM init (hGetLine handler2)
		hPutStrLn handler1 msg2

		writeChan chan msg2
		loop)

	handle (\(SomeException _) -> return () ) $ fix $ \loop -> do
		line <- readChan chan
		case line of
			"quit" -> killThread reader >> endMsg handler1 >> endMsg handler2
			_      -> loop

	hClose handler1
	hClose handler2
