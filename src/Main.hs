module Main where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss

import System.Environment
import System.IO

import Board
import Draw
import Input
import AI

-- 'play' starts up a graphics window and sets up handlers for dealing
-- with inputs and updating the world state.
--
-- 'drawWorld' converts the world state into a gloss Picture
--
-- 'handleInput' is called whenever there is an input event, and if it is
-- a human player's turn should update the board with the move indicated by
-- the event
--
-- 'updateWorld' is called 10 times per second (that's the "10" parameter)
-- and, if it is an AI's turn, should update the board with an AI generated
-- move

main :: IO ()
main = do   args <- getArgs
            pictures <- getBoard
            let flags = getArguments args
            initWorld <- makeWorld flags
            playIO (InWindow "Gomoku" (820, 820) (10, 10)) white 10
                initWorld -- in Board.hs
                (drawWorld pictures)-- in Draw.hs
                handleInput -- in Input.hs
                updateWorld -- in AI.hs

getArguments :: [String] -> Flags
getArguments args = foldl (\(Flags h w ) str -> case str of
                                                  "-h" -> (Flags True w ) --added
                                                  "-w" -> (Flags h True)
                                                  _    -> (Flags h w)) (Flags False False) args

getBoard :: IO [Picture]
getBoard = do board <- loadBMP "./Pictures/b.bmp"; return board
              white <- loadBMP "./Pictures/w1o.bmp"
              black <- loadBMP "./Pictures/b1o.bmp"
              hint <- loadBMP "./Pictures/hint.bmp"
              return [board, white, black, hint]