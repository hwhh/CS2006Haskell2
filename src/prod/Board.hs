module Board where

import Data.List
import Debug.Trace
import Data.Maybe
import System.Random
import Data.Tuple


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
                     previous_board :: Maybe Board,
                     lines :: [[(Position)]]
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
                    Just b -> return $ world { board = fromJust pb,  turn = (other (turn world))}
                    Nothing -> return $ world


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
                                      (if t then 4 else 5)
                                      []
                                      (False, Nothing)
                                      (Nothing)
                                      (createLines (if bs then 6 else 15) (if t then 4 else 5))
                                      --((3,3),Black), ((3,2),Black),((4,4),White),((3,4),White)


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


createLines :: Int -> Int -> [[Position]]
createLines s t= init $ foldr(\p acc -> (createLine s s (p:[])):acc ) [[]] $ createDirs s l
               where l = (s+1) - t

-- |Genereates the first cell and a direction for every line > target on the board
createDirs ::Int -> Int -> [(Position, Direction)]
createDirs s l = zip (zip [0..s] (repeat 0)) (repeat (0,1))++ --N && S
                 zip (zip (repeat 0) [0..s]) (repeat (1,0)) ++ --W && E
                 zip (zip [l,l-1..0] (repeat 0)) (repeat (1,1)) ++ zip (zip (repeat 0) [1,2..l]) (repeat (1,1))++
                 zip (zip (repeat 0) [s,s-1..s-l]) (repeat (1,-1)) ++ zip (zip [1,2..l] (repeat s)) (repeat (1,-1))



-- |Given a position and direction generate the whole line
createLine :: Int -> Int -> [(Position, Direction)] -> [Position]
createLine s 0 pos     = filter (\(x,y) -> (x<=s && x>=0) && (y<=s && y>=0)) $ map fst pos
createLine s n (p:pos) = createLine s (n-1) (((p1+d1, p2+d2),(d1, d2)):(p:pos))
                       where d1 = fst (snd p )
                             d2 = snd (snd p)
                             p1 = fst (fst p)
                             p2 = snd (fst p)



-- |Makes a move on the board
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove b c p
            | fst (won b) = Nothing -- Checks in game is over
            | fst p  < s && snd p < s  && fst p  > -1 && snd p > -1 =  -- Checks pieces in on the board
                                            case elem p (map fst (pieces b)) of
                                                True -> Nothing
                                                False -> case checkWon (b {pieces =  ((p, c):pieces b)}) c of
                                                            True -> Just (b {pieces =  ((p, c):pieces b), won=(True, Just c), previous_board=Just pd})
                                                            False ->Just (b {pieces =  ((p, c):pieces b), previous_board=Just pd})
            | otherwise = Nothing

            where s = (fromIntegral $ size b)+1
                  score = evaluate b c
                  pd = b


-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won


-- |Checks if the game has been won
checkWon :: Board -> Col -> Bool
checkWon b c = if checkLines b c (target b) (Board.lines b) == 1 then True else False



-- |checks all the lines in every directions
checkLines :: Board -> Col -> Int -> [[Position]] -> Int
checkLines _ _ _ [] = 0
checkLines b c target (x:xs) | maximum (map fst (checkLine b c x)) == target = 1
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
scoreLine :: Board -> [(Int, (Position, Maybe Col))] -> Maybe Col -> Col -> Bool -> Int -> Int
scoreLine b (x:xs) prev c first score  |length (x:xs) <= (target b) = case maximum (map fst max) == (target b) of  -- checks if n free slots in a row
                                                                           True ->  if (checkOpening c (prev, Nothing)) == 2
                                                                                        then let new_score = (length $ filter  (== Just c) (map snd max)) in  score + new_score -- Checks the opeings by getting the previous and the next cells
                                                                                        else score
                                                                           False -> score
                                       |first                       =  case maximum (map fst max) == (target b) of -- Checks if n free slots in a row
                                                                            True ->if (checkOpening c (prev, snd (snd ((x:xs) !! (length next_x))))) ==2
                                                                                       then let new_score = (length $ filter  (== Just c) (map snd max)) in
--                                                                                             if new_score == 0
--                                                                                                then  scoreLine b (take (target b) xs) (snd (snd (xs!!((target b)-1)))) c False score
--                                                                                             else
                                                                                             if (new_score >= ((target b) -  1))
                                                                                                then scoreLine b xs (snd (snd (x))) c False (score + new_score^2 ) --in trace (show xp) xp
                                                                                             else
                                                                                                scoreLine b xs (snd (snd (x))) c False (score + new_score)   -- Checks the opeings by getting the previous and the next cells

                                                                                       else scoreLine b xs (snd (snd (x))) c False score
                                                                            False -> scoreLine b xs (snd (snd (x))) c False score
                                       |otherwise                   = case maximum (map fst max) == (target b) of
                                                                            True -> if (checkOpening c (prev, snd (snd ((x:xs) !! (length next_x))))) ==2
                                                                                        then let new_score = (length $ filter  (== Just c) (map snd max)) in
--                                                                                              if new_score == 0
--                                                                                                    then  scoreLine b (take (target b) xs) (snd (snd (xs!!((target b)-1)))) c False score
--                                                                                              else
                                                                                              if (new_score >= ((target b) -  1))
                                                                                                    then scoreLine b xs (snd (snd (x))) c False (score + new_score^2 ) --in trace (show xp) xp
                                                                                              else
                                                                                                    scoreLine b xs (snd (snd (x))) c False (score + new_score)   -- Checks the opeings by getting the previous and the next cells
                                                                                        else scoreLine b xs (snd (snd (x))) c False score
                                                                            False -> scoreLine b xs (snd (snd (x))) c False score

                                where next_x = take (target b) (x:xs)
                                      max  =  sumList next_x c


--

--getScore :: Board -> Col -> [[Position]] -> Int
--getScore b c lines = foldl(\acc x -> let l = checkLine b c x in  acc + scoreLine b l Nothing c True 0
--
--
--                                   ) 0 lines
--                            where s = (size b)
--

getScore :: Board -> Col -> [[Position]] -> Int
getScore b c lines = foldl(\acc x -> case length (pieces b) >= 4 of
                                            True -> if p1 `elem` x || p2 `elem` x || p3 `elem` x ||  (fst $ (pieces b)!!3) `elem` x then
                                                        let l = checkLine b c x in trace("here" ++ show (fst $ (pieces b)!!3)) acc + scoreLine b l Nothing c True 0
                                                    else acc
                                            False ->if p1 `elem` x || p2 `elem` x || p3 `elem` x then
                                                        let l = checkLine b c x in trace(show p3) acc + scoreLine b l Nothing c True 0
                                                    else acc
                                   ) 0 lines
                            where s = (size b)
                                  p1 = fst $ (pieces b)!!0
                                  p2 = fst $ (pieces b)!!1
                                  p3 = fst $ (pieces b)!!2


-- |Evalutes the board for a given player
evaluate :: Board -> Col -> Int
evaluate b col | fst (won b) && snd (won b) == Just col = (maxBound :: Int) -- Checks if won or loss
               | fst (won b) && snd (won b) == Just (other col) = (minBound :: Int)--  Checks if won or loss
               | otherwise =  (getScore b col (Board.lines b)) - (getScore b (other col) (Board.lines b))










--getScore :: Board -> Col -> [[Position]] -> Int
--getScore b c lines = foldl(\acc x -> if p1 `elem` x || p2 `elem` x || p3 `elem` x then let l = checkLine b c x in acc + scoreLine b l Nothing c True 0 else acc
--
--                                   ) 0 lines
--                            where s = (size b)
--                                  p1 = fst $ head (pieces b)
--                                  p2 = fst $ head $ tail (pieces b)
--                                  p3 = fst $ head $ tail $ tail (pieces b)




--scoreSegmeant ::  Board -> [(Int, Maybe Col)] -> Maybe Col -> Maybe Col  -> Col -> Int -> Int
--scoreSegmeant b max prev after col score  = if (checkOpening c (prev, after)) ==2 then
--                                                let new_score = (length $ filter  (== Just c) (map snd max)) in
--                                                     if new_score == 0 then
--                                                        score --scoreLine b (take (target b) xs) (snd (snd (xs!!((target b)-1)))) c False
--                                                    else if (new_score >= ((target b) -  1)) then
--                                                        (score + new_score^2 ) --  scoreLine b xs (snd (snd (x))) c False
--                                                    else
--                                                        (score + new_score) --scoreLine b xs (snd (snd (x))) c False  -- Checks the opeings by getting the previous and the next cells
--                                            else score --scoreLine b xs (snd (snd (x))) c False
--
---- |checks if player can win with exactly x in a row
--scoreLine :: Board -> [(Int, (Position, Maybe Col))] -> Maybe Col -> Col -> Bool -> Int -> Int
--scoreLine b (x:xs) prev c first score  |length (x:xs) <= (target b) = case maximum (map fst max) == (target b) of  -- checks if n free slots in a row
--                                                                           True -> scoreSegmeant b max prev Nothing c score
--                                                                           False -> score
--                                       |first                       =  case maximum (map fst max) == (target b) of -- Checks if n free slots in a row
--                                                                            True ->scoreLine b xs (snd (snd (x))) c False (scoreSegmeant b max Nothing (snd (snd (x:xs!!(target b)))) c score)
--                                                                            False -> scoreLine b xs (snd (snd (x))) c False score
--                                       |otherwise                   = case maximum (map fst max) == (target b) of
--                                                                            True -> scoreLine b xs (snd (snd (x))) c False (scoreSegmeant b max prev (snd (snd (x:xs!!(target b)))) c score)
--                                                                            False -> scoreLine b xs (snd (snd (x))) c False score
--
--                                where next_x = take (target b) (x:xs)
--                                      max  =  sumList next_x c