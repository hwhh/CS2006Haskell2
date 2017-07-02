{-# LANGUAGE BangPatterns #-}
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
            pictures <- getPictures
            let flags = getSetting args
            initWorld <- makeWorld flags
            playIO (InWindow "Gomoku" (820, 820) (10, 10)) white 10
                initWorld -- in Board.hs
                (drawWorld pictures)-- in Draw.hs
                handleInput -- in Input.hs
                updateWorld -- in AI.hs

-- |Gets the program arguments
getSetting :: [String] -> Flags
getSetting args = foldl (\(Flags bs t h w p) str -> case str of
                                                          "-bs" -> (Flags True t h w p)
                                                          "-t" -> (Flags bs True h w p)
                                                          "-h" -> (Flags bs t True w p ) --added
                                                          "-w" -> (Flags bs t h True p)
                                                          "-p" -> (Flags bs t h w True)
                                                          _    -> (Flags bs t h w p)) (Flags False False False False False) args

-- |Loads the pictures
getPictures :: IO [Picture]
getPictures = do  board <- loadBMP "./Pictures/b.bmp"; return board
                  white <- loadBMP "./Pictures/w1o.bmp"
                  black <- loadBMP "./Pictures/b1o.bmp"
                  hint <- loadBMP "./Pictures/hint.bmp"
                  return [board, white, black, hint]