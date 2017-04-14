module Board where

import Data.List
import Debug.Trace
import Data.Maybe

dirs = [(0.0, -1.0), (1.0, -1.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0), (-1.0, 1.0), (-1.0, 0), (-1.0, -1.0)]
--          0             1             2           3           4           5           6           7
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

            where s = (fromIntegral $ size b)+1


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
checkWon b c = if checkLines b c (target b) combinations == 1 then True else False
              where combinations = createLines b -- [(x, y) | x <-  map (fst) (filter ((==c).snd) (pieces b)), y <- dirs]


checkLines :: Board -> Col -> Int -> [(Position, Direction)] -> Int
checkLines _ _ _ [] = 0
checkLines b c target (x:xs) | checkLine target b c (createLine b s (x:[])) == target = 1
                             | otherwise = checkLines b c target xs
                             where s = (size b)


checkLine :: Int -> Board -> Col -> [Position] -> Int
checkLine target b c pos = foldr (\(x,y) acc ->
                            case ((x,y),c) `elem` (pieces b) of
                                  True -> acc+1
                                  False -> if acc >= target then acc else 0) 0 pos

evaluate :: Board -> Col -> Int
evaluate b col = (checkLines b col 2 combinations)*100
               + (checkLines b col 3 combinations)*1000
               + (checkLines b col 4 combinations)*(-100)
                where max = maxBound :: Int
                      s = fromIntegral (size b)
                      combinations = createLines b


createLines ::Board -> [(Position, Direction)]
createLines b = zip (zip [0..s] (repeat 0)) (repeat (0,1))++ --N && S
                zip (zip (repeat 0) [0..s]) (repeat (1,0)) ++ --W && E
                zip (zip [l,l-1..0] (repeat 0)) (repeat (1,1)) ++zip (zip (repeat 0) [1..l]) (repeat (1,1))++ -- NE && SW
                zip (zip (repeat 0) [l-1..s]) (repeat (1,-1))++zip (zip [1..l] (repeat 5)) (repeat (1,-1))  -- NW && SE
               where s = fromIntegral $ size b
                     l = s - (fromIntegral $ target b)

createLine :: Board -> Int -> [(Position, Direction)] -> [Position]
createLine b 0 pos     = filter (\(x,y) -> (x<=s && x>=0) && (y<=s && y>=0)) $ map fst pos
                       where s = fromIntegral (size b)
createLine b n (p:pos) = createLine b (n-1) (((p1+d1, p2+d2),(d1, d2)):(p:pos))
                       where d1 = fst (snd p )
                             d2 = snd (snd p)
                             p1 = fst (fst p)
                             p2 = snd (fst p)
