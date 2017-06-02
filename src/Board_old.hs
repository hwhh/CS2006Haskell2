module Board where

import Data.List
import Debug.Trace
import Data.Maybe
import System.Random

dirs = [(0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)]
--          0             1             2           3           4           5           6           7

data Col = Black | White
  deriving (Show, Eq)

other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)
type Direction = (Int, Int)


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
                     won :: (Bool, Maybe Col),
                     previous_board :: Maybe Board
                   }
  deriving Show


data Flags = Flags { bs :: Bool, -- szie of board -b
                     t :: Bool, -- target -t
                     hardAi :: Bool, -- HardAi           -h
                     whiteP :: Bool, -- Player is White  -w
                     pvp :: Bool -- Player vs Player     -h
                     }
 deriving Show

instance Read Col where
readsPrec d "White" = [(White, "")]
readsPrec d "Black" = [(Black, "")]

-- |Undoes the last move
undo :: World -> IO World
undo world = let pb = previous_board (board world)
             in case pb of -- Checks theres a previous world
                    Just b -> return $ world { board = fromJust pb, turn = (other (turn world)) }
                    Nothing -> return $ world
-- Default board is 6x6, target is 3 in a row, no initial pieces

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { flags :: Flags,
                     board :: Board,
                     turn :: Col,
                     hints :: Bool,
                     game_over :: Bool,
                     h_player :: Col,
                     ai_level :: Int,
                     last_move :: Maybe (Position, Col),
                     pVp :: Bool
                     }
 deriving Show



initBoard :: Flags -> Board
initBoard (Flags bs t _ _ _ ) = Board (if bs then 6 else 15)
                                      (if t then 3 else 5)
                                      [] (False, Nothing) (Nothing)

-- |Creates an IO World
makeWorld :: Flags -> IO World
makeWorld (Flags bs t h w p ) = do return(initWorld (Flags bs t h w p ) (initBoard (Flags bs t h w p )) )

-- |Creates the inital world
initWorld :: Flags -> Board -> World
initWorld (Flags bs t h w p) board = World   (Flags  bs t h w p)
                                             board
                                             Black
                                             False
                                             False
                                             (if w then White else Black)
                                             (if h then 3 else 1)
                                             Nothing
                                             (if p then True else False)

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)

-- |Generares a random tuple
getRandomTuple :: Int -> IO (Int,Int)
getRandomTuple size = do
       randomX <- randomRIO (1, size-1)
       randomY <- randomRIO (1, size-1)
       return (randomX, randomY)


-- |Makes a move on the board
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove b c p
            | fst (won b) = Nothing -- Checks in game is over
            | fst p  < s && snd p < s  && fst p  > -1 && snd p > -1 =  -- Checks pieces in on the board
                                            case elem p (map fst (pieces b)) of
                                                True -> Nothing
                                                False ->case checkWon (b {pieces =  ((p, c):pieces b)}) c of
                                                            True -> Just (b {pieces =  ((p, c):pieces b), won=(True, Just c), previous_board=Just pd})
                                                            False ->Just (b {pieces =  ((p, c):pieces b), previous_board=Just pd})
            | otherwise = Nothing

            where s = (fromIntegral $ size b)+1
                  score = evaluate b c
                  pd = b


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

-- |Checks if the game has been won
checkWon :: Board -> Col -> Bool
checkWon b c = if checkLines b c (target b) combinations == 1 then True else False
              where combinations = createLines b


-- |checks all the lines in every directions
checkLines :: Board -> Col -> Int -> [(Position, Direction)] -> Int
checkLines _ _ _ [] = 0
checkLines b c target (x:xs) | maximum (map fst (checkLine b c (createLine b s (x:[])))) == target = 1
                             | otherwise = checkLines b c target xs
                             where s = (size b)

-- |Calculates the total number of adjacent cells
checkLine :: Board -> Col -> [Position] -> [(Int, (Position, Maybe Col))]
checkLine b c pos  = tail $ scanl (\(s,(p,col)) (x,y)  ->  if ((x,y),c) `elem` (pieces b) then (s+1, ((x,y), Just c))
                                                              else if ( ((x,y),(other c)) `elem` (pieces b) ) then (0, ((x,y), Just (other c)))
                                                              else (0, ((x,y), Nothing))
                               ) (0, ((pos!!0), Nothing)) pos

-- |Calculates the number of adjacent cells  that are free or occupied by a colour
sumList ::[(Int, (Position, Maybe Col))] -> Col -> [(Int, Maybe Col)]
sumList posses c =  tail $ scanl (\(a1, a2) (s,(p,col))  -> case col of
                                                Just col -> if col == c then (a1+1, Just col) else (0, Just (other col))
                                                Nothing -> (a1+1, Nothing)
                      ) (0, Nothing) posses


-- |check wether there are adjacent cells
checkOpening :: Col-> (Maybe Col, Maybe Col) -> Int
checkOpening col cols |cols == (Just col, Just col) = 0
                      |Just col == fst cols || Just col == snd cols=  1
                      |otherwise =  2

-- |checks if player can win with exactly x in a row
checkWinnable :: Board -> [(Int, (Position, Maybe Col))] -> Maybe Col -> Col -> Bool -> Bool
checkWinnable b (x:xs) prev c first   |length (x:xs) <= (target b) = case maximum max == (target b) of  -- checks if n free slots in a row
                                                                           False -> False
                                                                           True ->  if (checkOpening c (prev, Nothing)) ==2 then True -- Checks the opeings by getting the previous and the next cells
                                                                                    else False
                                      |first                       =  case maximum max == (target b) of -- Checks if n free slots in a row
                                                                          False -> checkWinnable b xs (snd (snd (x))) c False
                                                                          True ->  if  (checkOpening c (Nothing, snd (snd ((x:xs) !! (length next_x))))) == 2 then True -- Checks the opeings by getting the previous and the next cells
                                                                                   else checkWinnable b xs (snd (snd (x))) c False
                                      |otherwise                   = case maximum max == (target b) of
                                                                         False -> checkWinnable b xs (snd (snd (x))) c False
                                                                         True ->  if (checkOpening c (prev, snd (snd ((x:xs) !! (length next_x))))) ==2 then True -- Checks the opeings by getting the previous and the next cells
                                                                                  else  checkWinnable b xs (snd (snd (x))) c False
                                where next_x = take (target b) (x:xs)
                                      max  =  map fst $ sumList next_x c

-- |Calculates the score based on how many in a row and if the row is winnable
getScore :: Board -> Col -> [(Position, Direction)] -> Int
getScore b c lines = foldl(\acc x -> let l = createLine b s (x:[]) -- Creates the line
                                         l' = checkLine b c l  in -- Analayses the line
                                        case checkWinnable b l' Nothing c True of
                                               True -> let max = sumList l' c in
                                                            if maximum (map fst max) >= (target b) && Just c `elem` map snd max then acc+maximum (map fst max) -- Gets the maximum number of adjacent solts that can be filled by colour
                                                            else acc + (sum $ map fst $ filter (\(s,(p,col)) -> col == Just c)l') -- Sums the number of adjacent pices for colour
                                               False -> acc
                                   ) 0 lines
                            where s = (size b)
-- |Gets a score for a player
getPlayersScore :: Board -> Col -> Int
getPlayersScore b col = getScore b col (createLines b)


-- |Evalutes the board for a given player
evaluate :: Board -> Col -> Int
evaluate b col = if fst (won b) && snd (won b) == Just col then (maxBound :: Int) -- Checks if won or loss
                 else if fst (won b) && snd (won b) == Just (other col) then (minBound :: Int)--  Checks if won or loss
                 else (getPlayersScore b col)-(getPlayersScore b (other col))

-- |Genereates the first cell and a direction for every line > target on the board
createLines ::Board -> [(Position, Direction)]
createLines b = zip (zip [0..s] (repeat 0)) (repeat (0,1))++ --N && S
                zip (zip (repeat 0) [0..s]) (repeat (1,0)) ++ --W && E
                zip (zip [l,l-1..0] (repeat 0)) (repeat (1,1)) ++zip (zip (repeat 0) [1..l]) (repeat (1,1))++
                zip (zip (repeat 0) [l..s]) (repeat (1,-1))++zip (zip [1..l] (repeat s)) (repeat (1,-1))
               where s = size b
                     l = s - (target b)


-- |Given a position and direction generate the whole line
createLine :: Board -> Int -> [(Position, Direction)] -> [Position]
createLine b 0 pos     = filter (\(x,y) -> (x<=s && x>=0) && (y<=s && y>=0)) $ map fst pos
                       where s = fromIntegral (size b)
createLine b n (p:pos) = createLine b (n-1) (((p1+d1, p2+d2),(d1, d2)):(p:pos))
                       where d1 = fst (snd p )
                             d2 = snd (snd p)
                             p1 = fst (fst p)
                             p2 = snd (fst p)

-- check if there is a way to form a 5 and win. And if there is not, the next should be to check if Your opponent can do that, and if yes, then defense
-- counting the number of bounded, unbounded and partially bounded continuous sequences of stones
