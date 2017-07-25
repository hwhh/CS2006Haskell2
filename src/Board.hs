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
                                      [((3,3),Black),((4,3),Black),((5,3),Black), ((6,3),White),((2,2),White),((2,3),White),((2,4),Black), ((4,2),White)]
                                      --[]
                                      (False, Nothing)
                                      (Nothing)
                                      (createLines (if bs then 6 else 15) (if t then 4 else 5))

                                      --((0,6),Black),((6,6),White),((0,5),Black),((6,5),White)

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


other :: Col -> Col
other Black = White
other White = Black

-- |Undoes the last move
getb:: Maybe Board -> World -> Col -> World
getb pb w c = case pb of -- Checks theres a previous world
                  Just b -> w { board = fromJust pb,  turn = c}
                  Nothing ->  w

undo :: World -> IO World
undo world = case (pVp world) of
                    True -> return $ getb (previous_board (board world)) world (other (turn world))
                    False -> if (length (pieces (board world))) > 2 then
                                return $ getb (previous_board (fromJust(previous_board (board world)))) world (turn world)
                             else
                                return world


createLines :: Int -> Int -> [[Position]]
createLines s t= init $ foldr(\p acc -> ((createLine s s (p:[]))):acc ) [[]] $ createDirs s l -- in trace (show x) x
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
                                                False -> let new_board = (b {pieces =  ((p, c):pieces b)})  in
                                                    case checkWon new_board c (Board.lines new_board) of
                                                            True -> Just (new_board { won=(True, Just c) ,previous_board=Just pd})
                                                            False ->Just (new_board {previous_board=Just pd})  --in trace (show (evaluate x c) ++ " "++ show c)
            | otherwise = Nothing

            where s = (fromIntegral $ size b)+1
                  pd = b

checkWon :: Board -> Col -> [[Position]] -> Bool
checkWon b c [] = False
checkWon b c (x:xs) | p1 `elem`(x) = if maximum (map fst (checkLine b c x)) == (target b) then True else checkWon b c xs
                    | otherwise = checkWon b c xs
                    where s = (size b)
                          p1 = fst $  (pieces b)!!0


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
sumList ::[(Position, Maybe Col)] -> Col -> [(Int, Maybe Col)]
sumList posses c =  tail $ scanl (\(a1, a2) (p,col)  -> case col of
                                                Just col -> if col == c then (a1+1, Just col) else (0, Just col)
                                                Nothing -> (a1+1, Nothing)
                      ) (0, Nothing) posses


-- |check wether there are adjacent cells
checkOpening :: Col-> (Maybe Col, Maybe Col) -> Int
checkOpening col cols |(fst cols /= Just col) && (snd cols /= Just col) = 2
                      |Just col == fst cols || Just col == snd cols=  1
                      |cols == (Just col, Just col) = 0


checkPartial :: Int -> Int -> Int -> Int -> Int
checkPartial score blocks edge target  = if score == 0 then (-1)
                                         else if (score == target) then  ((score^6) - blocks) - edge
                                         else if (score == target-1) then  ((score^3) - blocks) - edge
                                         else (score - blocks) - edge


scorePartialLine' ::  Board  -> Maybe Col -> Maybe Col -> Col -> [(Int, Maybe Col)] -> Int -> Bool -> Int
scorePartialLine' b prev next c max score edge | maximum (map fst max) == (target b) && (checkOpening c (prev, next)) == 2  && edge  = checkPartial new_score no_of_blocks 1 (target b)
                                               | maximum (map fst max) == (target b) && (checkOpening c (prev, next)) == 2  && (not edge)  = checkPartial new_score no_of_blocks 0 (target b)
                                               | otherwise = score
                                               where new_score = (length $ filter (== Just c) (map snd max))
                                                     no_of_blocks = checkOpening (other c) (prev, next)


--Add score for blocking ?
-- |checks if player can win with exactly x in a row
scoreLine :: Int -> Board -> [(Position, Maybe Col)] -> Maybe Col -> Col -> Bool -> Int -> Int
scoreLine level _ [] _ _  _ score = score
scoreLine level b (x:xs) prev c edge score  |length (x:xs) == (target b) = let new_score = scorePartialLine' b prev Nothing c max score True in
                                                                                if new_score == (-1)
                                                                                    then score - (level)
                                                                                else
                                                                                   new_score - (level)
                                              |otherwise                 = let new_score = scorePartialLine' b prev (snd ((x:xs) !! (target b))) c max score edge in
                                                                                if new_score == (-1)
                                                                                     then scoreLine level b xs (snd (x)) c False score --(dropWhile(\ac -> (snd ac) == Nothing) (xs))
                                                                                else
                                                                                    scoreLine level b xs (snd (x)) c False new_score

                                      where next_x = take (target b) (x:xs)
                                            max  =  sumList next_x c

--Have to distingwish between edge and nothing i.e. can i keep buliding in that direction - Also check if being blocked i.e the number of blocks adds to the score


getScore ::Int -> Board -> Col -> [[Position]] -> Int
getScore level b c lines = foldl(\acc x -> let occupied = intersect x positions in
                                            case length occupied > 0 of
                                                True ->  acc + scoreLine level b (map snd (checkLine b c x)) Nothing c True 0
                                                False -> acc
                                   ) 0 lines
                            where s = (size b)
                                  positions = map fst (pieces b)


-- |Evalutes the board for a given player
evaluate ::Int ->  Board -> Col -> Int
evaluate level b col | fst (won b) && snd (won b) == Just col = 1000000 -- Checks if won or loss
                     | fst (won b) && snd (won b) == Just (other col) = (-1000000)
                     | otherwise =  (getScore level b col (Board.lines b)) - (getScore level b (other col) (Board.lines b))





-- 1. blocks = 0 and edge
-- 2. blocks = 0 and edge and (target -1)
--
-- 1. blocks = 1 and edge
-- 2. blocks = 1 and edge and (target -1)
--
-- 3. blocks = 2 and not edge
-- 4. blocks = 2 and not edge and (target -1)
--
-- 5. blocks = 1 and not edge
-- 6. blocks = 1 and not edge and (target -1)
--
-- 7. blocks = 0 and not edge
-- 8. blocks = 0 and not edge and (target -1)


-- scorePartialLine :: Int -> Board  -> Maybe Col -> Maybe Col -> Col -> [(Int, Maybe Col)] -> Int -> Bool -> Int
-- scorePartialLine level b prev next c max score edge =case maximum (map fst max) == (target b) of -- Checks if n free slots in a row
--                                                         True -> if (checkOpening c (prev, next)) == 2 then
--
--                                                                          if new_score == 0 then (-1)
--
--                                                                          else if (new_score >= (target b)-1) then (score+new_score^4) -- `div` level+1
--                                                                          else if (new_score >= (target b)-1 && edge) then  (score+new_score^3) -- `div` level+1
--
--
--                                                                          else (score+new_score) `div` level+1
--                                                                else score
--                                                         False -> score
--                                                      where new_score = (length $ filter (== Just c) (map snd max))

