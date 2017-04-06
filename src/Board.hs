module Board where

import Data.List
import Debug.Trace



data Col = Black | White
  deriving (Show, Eq)

other :: Col -> Col
other Black = White
other White = Black

type Position = (Float, Float)
type Direction = (Float, Float)


-- A Board is a record containing the board size (a board is a square grid,
-- n * n), the number of pieces in a row required to win, and a list 
-- of pairs of position and the colour at that position.  So a 10x10 board 
-- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- would be represented as:
--
-- Board 10 5 [((5, 5), Black), ((8,7), White)]

data Board = Board { size :: Int,
                     target :: Int,
                     pieces :: [(Position, Col)]
                   }
  deriving Show

-- Default board is 6x6, target is 3 in a row, no initial pieces
initBoard = Board 6 3 [((0, 0), White)]

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board,
                     turn :: Col }


initWorld = World initBoard Black

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)

makeMove :: Board -> Col -> Position -> Maybe Board
makeMove b c p
            | fst p  < s && snd p < s =  case elem p (map fst (pieces b)) of
                                                True -> Nothing
                                                False -> Just b {pieces =  ((p, c):pieces b)}
            | otherwise = Nothing

            where s = fromIntegral $ size b



-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Col
checkWon b = undefined -- any (\row -> all())
--any (\row -> all (\col -> b!(row,col) == t) [1..3]) [1..3]


check :: Board -> Col -> Bool
check b c = undefined


func3 :: Board -> Position -> Direction -> Maybe [Position]
func3 b p d | end_x > s  || end_y > s = Nothing
            | d == (0,-1) = Just (zip (repeat (fst p)) [snd p, snd p-1 .. end_y])-- N
            | d == (1,-1) = Just (zip [fst p .. end_x] [snd p, snd p-1 .. end_y])-- NE
            | d == (1,0)  = Just (zip [fst p .. end_x] (repeat (snd p)))-- E
            |  == (1,0)  = Just (zip [fst p .. end_x] (repeat (snd p)))-- E


            where end_x = fst p + (fromIntegral (target b) * fst d)
                  end_y = snd p + (fromIntegral (target b) * snd d)
                  s = (fromIntegral (size b))

--generateLines :: Board -> Position -> Direction -> Maybe [(Position)]
--generateLines b d p | end_x > s  || end_y > s = Nothing
--                    | fst d == snd d = Just(zip [fst p .. end_x] [fst p .. end_y])
--                    | otherwise = Just([(x,y) | x <- [fst p .. end_x] , y <- [snd p .. end_y]])
--                    where end_x = fst p + (fromIntegral (target b) * fst d)
--                          end_y = snd p + (fromIntegral (target b) * snd d)
--                          s = (fromIntegral (size b))


{- Hint: One way to implement 'checkWon' would be to write functions 
which specifically check for lines in all 8 possible directions
(NW, N, NE, E, W, SE, SW)

In these functions:
To check for a line of n in a row in a direction D:
For every position ((x, y), col) in the 'pieces' list:
- if n == 1, the colour 'col' has won
- if n > 1, move one step in direction D, and check for a line of n-1 in a row.
-}

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate = undefined



