module NetworkStuff where

import System.IO
import Data.List
import Board


createStr :: World -> String
createStr world =
	let
		b = board world
	in
		intercalate "**" [show (size b), show (target b), unwords [ show x ++ " " | x <- pieces b ],
			show (won b), show (turn world), show (game_over world),
			show (h_player world), show (ai_level world), show (last_move world) ++ " "]
			
getWorld :: [String] -> World -> World
getWorld xs world =
	let
		b = Board { size = read (xs !! 0) :: Int,
	 		target = read (xs !! 1) :: Int,
			pieces = [ read x :: ((Int, Int), Col) | x <- words (xs !! 2) ],
			won = read (xs !! 3) :: (Bool, Maybe Col),
			previous_board = Nothing }
	in
		world { board = b,
			turn = read (xs !! 4) :: Col,
			hints = False,
			game_over = read (xs !! 5) :: Bool,
			h_player = read (xs !! 6) :: Col,
			ai_level = read (xs !! 7) :: Int,
			last_move = read (xs !! 8) :: Maybe ((Int, Int), Col) }
