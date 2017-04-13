module Board where

import Data.List
import Debug.Trace
import Data.Maybe

dirs = [(0.0, -1.0), (1.0, -1.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0), (-1.0, 1.0), (-1.0, 0), (-1.0, -1.0)]

data Cell = B | W | E
    deriving (Show, Eq)

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
                     pieces :: [(Position, Col)],
                     won :: (Bool, Maybe Col)
                   }
  deriving Show




-- Default board is 6x6, target is 3 in a row, no initial pieces
initBoard = Board 6 3 [] (False, Nothing)

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board,
                     turn :: Col,
                     last_move :: Maybe (Position, Col)
                     }
 deriving Show

data AI = AI {col :: Col,
              previous_moves :: [(Position, Col)]}

initWorld = World initBoard Black Nothing


-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)

makeMove :: Board -> Col -> Position -> Maybe Board
makeMove b c p
            | fst (won b) = Nothing
            | fst p  < s && snd p < s  && fst p  > -1 && snd p > -1 =
                                            case elem p (map fst (pieces b)) of
                                                True -> Nothing
                                                False -> case checkWon (b {pieces =  ((p, c):pieces b)}) c of
                                                                True -> Just (b {pieces =  ((p, c):pieces b), won=(True, Just c)})
                                                                False -> Just (b {pieces =  ((p, c):pieces b)})
            | otherwise = Nothing

            where s = fromIntegral $ size b



-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won

{- Hint: One way to implement 'checkWon' would be to write functions
which specifically check for lines in all 8 possible directions
(NW, N, NE, E, W, SE, SW)

In these functions:
To check for a line of n in a row in a direction D:
For every position ((x, y), col) in the 'pieces' list:
- if n == 1, the colour 'col' has won
- if n > 1, move one step in direction D, and check for a line of n-1 in a row.
-}

checkWon :: Board -> Col -> Bool
checkWon b c = if checkPositions b c (target b) combinations == 1 then True else False
              where combinations = [(x, y) | x <-  map (fst) (filter ((==c).snd) (pieces b)), y <- dirs]


checkPositions :: Board -> Col -> Int -> [(Position, Direction)] -> Int
checkPositions _ _ _ [] = 0
checkPositions b c target (x:xs) | checkPosition b p d c target == True = 1
                                 | otherwise = checkPositions b c target xs --(filter (`notElem` (map (\(x,y) -> ((x,y),d)) $ createLine b p d)) xs)
                                where p = fst x
                                      d = snd x

checkPosition :: Board -> Position -> Direction -> Col -> Int -> Bool
checkPosition b p d c 0 | p `notElem` map (fst) (pieces b) && onBoard p = True
                        | otherwise = False
checkPosition b p d c n | p `notElem` map (fst) (filter ((==c).snd) (pieces b)) = False
                        | otherwise = checkPosition b (fst p + fst d, snd p + snd d) d c (n-1)

onBoard :: Position -> Bool
onBoard p = if (fst p >= 0 && fst p < 6) && (snd p >= 0 && snd p < 6) then True else False

getX :: Position -> Float -> Position
getX (x, 0) d = (x, 0)
getX p d = getX (fst p + d, snd p -1) d

getY :: Position -> Float -> Position
getY (0,y) d = (0, y)
getY p d = getX (fst p -1, snd p + d) d

createLine ::Board -> Position -> Direction -> [Position]
createLine b p d | d == (1.0, -1.0) || d == (-1.0, 1.0)  = let x = fst $ getX p 1    in filter (\(x,y) -> x < s+1) $ zip [x, x-1 ..0] [0..s]
                 | d == (1.0, 1.0)  || d == (-1.0, -1.0) = let x = fst $ getX p (-1) in filter (\(x,y) -> x > -1) $ zip [x, x+1 ..5] [0..s]
                 | d == (0.0, -1.0) || d == (0.0, 1.0)   = let x = fst $ getX p 1    in zip [0..s] $ repeat $ snd p
                 | d == (-1.0, 0.0) || d == (1.0, 0.0)   = let x = fst $ getX p 1    in zip (repeat $ snd p) [0..s]
               where s = fromIntegral $ size b


-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.



scorePartialLine :: Board -> Col -> Int ->  [(Position, Direction)] -> Int
scorePartialLine b col no combinations = undefined

evaluate :: Board -> Col -> Int
evaluate b col = (checkPositions b col 2 combinations)*20
                 + (checkPositions b col 3 combinations)*30
                 -- + (checkPositions b col 4 combinations)*40

                where max = maxBound :: Int
                      combinations = [(x, y) | x <-  map fst (filter ((==col).snd) (pieces b)), y <- dirs]







--
--getNextPos:: Int -> Float -> Float
--getNextPos count dir | dir == 0 = 0
--                     | dir <  0 = dir - (fromIntegral $ count)
--                     | dir >  0 = dir + (fromIntegral $ count)
--
--
--checkClosed :: Board -> Col -> (Position, Direction) -> Int -> Bool
--checkClosed _ _ _ 0 = False
--checkClosed b c (p,d) n | (next_pos, other c) `elem` (pieces b) = True
--                        | otherwise = checkClosed b c (next_pos,d) (n-1)
--                        where next_pos = (fst p + fst d, snd p + snd d)



--checkWinPossible :: Int-> [Position] -> (Position, Direction) -> Bool
--checkWinPossible 2 _ _ = True
--checkWinPossible count all_p (p,d) | y `notElem` all_p = False
--                                   | otherwise = checkWinPossible (count+1) all_p (p,d)
--                                   where x = (fst p + (getNextPos count (fst d)), snd p + (getNextPos count (snd d)))
--                                         y = trace (show x) x




--                      acc = foldr (\x acc-> evalulateFullLine b 100 (pieces b) col x) 0 dirs
-- foldr (\((p1, p2),c) acc -> if ((p1+fst dir, p2+snd dir),c) `elem` (pieces b) then acc+score else 0) 1 pos



--evalulateFullLine :: Board -> Col-> Int -> Int -> Position -> Direction -> Int
--evalulateFullLine _ _ 300 _ _ _ = 300
--evalulateFullLine b c n count p d | (fst p + (getNextPos count (fst d)), snd p + (getNextPos count (snd d))) `notElem` map (fst) (filter ((==c).snd) (pieces b)) =  n
--                                  | otherwise = evalulateFullLine b c (n+100) (count+1) p d
