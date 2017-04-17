--module Board where
--
--import Data.List
--import Debug.Trace
--import Data.Maybe
--import Data.List.Split
--
--dirs = [(0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)]
----          0             1             2           3           4           5           6           7
--data Cell = B | W | E
--    deriving (Show, Eq)
--
--data Col = Black | White
--  deriving (Show, Eq)
--
--other :: Col -> Col
--other Black = White
--other White = Black
--
--type Position = (Int, Int)
--type Direction = (Int, Int)
--
--
---- A Board is a record containing the board size (a board is a square grid,
---- n * n), the number of pieces in a row required to win, and a list
---- of pairs of position and the colour at that position.  So a 10x10 board
---- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
---- would be represented as:
----
---- Board 10 5 [((5, 5), Black), ((8,7), White)]
--
--data Board = Board { size :: Int,
--                     target :: Int,
--                     pieces :: [(Position, Col)],
--                     won :: (Bool, Maybe Col)
--                   }
--  deriving Show
--
--
--data Flags = Flags { hardAi :: Bool, -- HardAi           -h
--                     whiteP :: Bool -- Player is White  -w
--                     }
-- deriving Show
--
--
--
--
---- Default board is 6x6, target is 3 in a row, no initial pieces
--
---- Overall state is the board and whose turn it is, plus any further
---- information about the world (this may later include, for example, player
---- names, timers, information about rule variants, etc)
----
---- Feel free to extend this, and 'Board' above with anything you think
---- will be useful (information for the AI, for example, such as where the
---- most recent moves were).
--data World = World { board :: Board,
--                     turn :: Col,
--                     hints :: Bool,
--                     game_over :: Bool,
--                     h_player :: Col,
--                     ai_level :: Int,
--                     last_move :: Maybe (Position, Col),
--                     previous_board :: Board
--
--                     }
-- deriving Show
--
--initBoard = Board 6 3 [] (False, Nothing)
--
--
--makeWorld :: Flags -> IO World
--makeWorld (Flags h w) = do return(initWorld (Flags h w) initBoard)
--
--
--initWorld :: Flags -> Board -> World
--initWorld (Flags h w) board = World  board
--                                     Black
--                                     False
--                                     False
--                                     (if w then White else Black)
--                                     (if h then 3 else 1)
--                                     Nothing
--                                     board
--
--
--
---- Play a move on the board; return 'Nothing' if the move is invalid
---- (e.g. outside the range of the board, or there is a piece already there)
--
--makeMove :: Board -> Col -> Position -> Maybe Board
--makeMove b c p
--            | fst (won b) = Nothing
--            | fst p  < s && snd p < s  && fst p  > -1 && snd p > -1 =
--                                            case elem p (map fst (pieces b)) of
--                                                True -> Nothing
--                                                False ->case checkWon (b {pieces =  ((p, c):pieces b)}) c of
--                                                            True  -> Just (b {pieces =  ((p, c):pieces b), won=(True, Just c)})
--                                                            False -> Just (b {pieces =  ((p, c):pieces b)})
--            | otherwise = Nothing
--
--            where s = (fromIntegral $ size b)+1
--                  score = evaluate b c
--
--
---- Check whether the board is in a winning state for either player.
---- Returns 'Nothing' if neither player has won yet
---- Returns 'Just c' if the player 'c' has won
--
--{- Hint: One way to implement 'checkWon' would be to write functions
--which specifically check for lines in all 8 possible directions
--(NW, N, NE, E, W, SE, SW)
--
--In these functions:
--To check for a line of n in a row in a direction D:
--For every position ((x, y), col) in the 'pieces' list:
--- if n == 1, the colour 'col' has won
--- if n > 1, move one step in direction D, and check for a line of n-1 in a row.
---}
--
--checkWon :: Board -> Col -> Bool
--checkWon b c = if checkLines b c (target b) combinations == 1 then True else False
--              where combinations = createLines b -- [(x, y) | x <-  map (fst) (filter ((==c).snd) (pieces b)), y <- dirs]
--
--
--checkLines :: Board -> Col -> Int -> [(Position, Direction)] -> Int
--checkLines _ _ _ [] = 0
--checkLines b c target (x:xs) | maximum (map fst (checkLine b c (createLine b s (x:[])))) == target = 1
--                             | otherwise = checkLines b c target xs
--                             where s = (size b)
--
--checkLine :: Board -> Col -> [Position] -> [(Int, (Position, Maybe Col))]
--checkLine b c pos  = tail $ scanl (\(s,(p,col)) (x,y)  ->  if ((x,y),c) `elem` (pieces b) then (s+1, ((x,y), Just c))
--                                                                  else if ( ((x,y),(other c)) `elem` (pieces b) ) then (0, ((x,y), Just (other c)))
--                                                                  else (0, ((x,y), Nothing))
--                               ) (0, ((pos!!0), Nothing))  pos
--
--sumList ::[(Int, (Position, Maybe Col))] -> Col -> [(Int, Bool)]
--sumList posses c =  tail $ scanl (\(a1, a2) (s,(p,col))  -> case col of
--                                                Just col -> if col == c then (a1+1, False) else (0, False)
--                                                Nothing -> (a1+1, a2)
--                      ) (0, True) posses
--
--checkOpening :: Col-> (Maybe Col, Maybe Col) -> Int
--checkOpening col cols |cols == (Just col, Just col) = 0
--                      |Just col == fst cols || Just col == snd cols=  1
--                      |otherwise =  2
--
----checkWinnable :: Board -> [(Int, (Position, Maybe Col))] -> Col -> Bool
----checkWinnable b (x:xs) c  | max == [] = False
----                          | length (x:xs) == s =let next_x = take (target b) (x:xs)
----                                                    last = snd (snd ((x:xs) !! ((length next_x)))) in
----                                                if maximum max /= (target b) then checkWinnable b xs c
----                                                else if checkOpening c (Nothing, last) <= 1 then False
----                                                else checkWinnable b xs c
----
----                          | length xs == t =    if maximum max /= (target b) then False
----                                                else if checkOpening c (prev, Nothing) == 0 then True
----                                                else False
----
----                          | otherwise         = let last = snd (snd ((xs) !! ((length next_x)))) in
----                                                 if maximum max /= (target b) then checkWinnable b xs c
----                                                 else if checkOpening c (prev, last) == 2 then True
----                                                 else checkWinnable b xs c
----
----                          where next_x = take (target b) (xs)
----                                prev = snd (snd x)
----                                max  =  map fst $ sumList next_x c
----                                t = (target b)
----                                s = (size b)+1
----
----getScore :: Board -> Col -> [(Position, Direction)] -> Int
----getScore b c lines = foldl(\acc x -> let l = createLine b s (x:[])
----                                         l' = checkLine b c l in
----                                        case checkWinnable b l' c of
----                                               True -> trace (show "here")acc + (sum $ map fst $ filter (\(s,(p,col)) -> col == Just c)l')
----                                               False -> acc
----                                   ) 0 lines
----                            where s = (size b)
--
--checkWinnable :: Board -> [(Int, (Position, Maybe Col))] -> Col -> Bool
--checkWinnable b (x:xs) c  | length (x:xs) == s = let    next_x = take (target b) (x:xs)
--                                                        last = snd (snd ((x:xs) !! ((length next_x)))) in
--                                                    if max == [] then trace ("Here10  False"++ show next_x ++ "  "++show prev ++ "  "++show last) False
--
--                                                    else if maximum max /= (target b)
--                                                        then trace ("Here11 Next "++ show next_x ++ "  "++show prev ++ "  "++show last) checkWinnable b xs c
--                                                    else if checkOpening c (Nothing, last) <= 1
--                                                        then trace ("Here13 True "++ show next_x ++ "  "++show prev ++ "  "++show last ++ "  "++show max) False
--                                                    else
--                                                         trace ("Here14 Next "++ show next_x ++ "  "++show prev ++ "  "++show last ++" " ++show (next_x) ) checkWinnable b xs c
--
--                          | length xs == t =  if max == [] then trace ("Here1 False"++ show next_x ++ "  "++show prev ++ "  "++show max) False
--                                             else if maximum max /= (target b)
--                                                 then trace ("Here2 False "++ show next_x ++ "  "++show prev ++ "  "++show max ++ " " ++show xs)False
--
--                                             else if checkOpening c (prev, Nothing) == 0
--                                                 then trace ("Here3 True "++ show next_x ++ "  "++show prev ++ "  "++show max++ " " ++show xs) True
--                                             else
--                                                 trace ("Here5 False"++ show next_x ++ "  "++show prev ++ "  "++show max++ " " ++show xs) False
--
--                          | otherwise      = let last = snd (snd ((xs) !! ((length next_x)))) in
--                                             if  max == [] then trace ("Here5.5 F "++ show next_x ++ "  "++show prev ++ "  "++show max ++ "  "++show last ++ "  ") False
--                                             else if maximum max /= (target b)
--                                                   then trace ("Here6 NEXT"++ show next_x ++ "  "++show prev ++ "  "++show max ++ "  "++show last ++ "  ") checkWinnable b xs c
--
--                                             else if checkOpening c (prev, last) == 2
--                                                   then trace ("Here7 True"++ show next_x ++ "  "++show prev ++ "  "++show max ++ "  "++show last ++ "  ")  True
--                                             else
--                                                  trace ("Here8 Next"++ show next_x ++ "  "++show prev ++ "  "++show max ++ "  "++show last ++ "  ")   checkWinnable b xs c
--                          where next_x = take (target b) (xs)
--                                prev = snd (snd x)
--                                max  =  map fst $ sumList next_x c
--                                empty = False `elem` (map snd (sumList next_x c))
--                                t = (target b)
--                                s = (size b)+1
--
--getScore :: Board -> Col  -> [(Position, Direction)] -> Int
--getScore b c  lines = let yy = foldr(\x acc -> let l = reverse $ [(6,6),(5,6),(4,6),(3,6),(2,6),(1,6),(0,6)]
--                                                   l' = checkLine b c l
--                                                   p = checkWinnable b l' c in
--                                                               if p then True-- acc + (sum $ map fst $ filter (\(s,(p,col)) -> col == Just c)l')
--                                                               else False--acc
--                                   ) False lines in trace (show yy ++" " ++show c) 10
--
--                            where s = (size b)
--getPlayersScore :: Board -> Col -> Int
--getPlayersScore b col = (getScore b col coms)*100
--                        where coms = createLines b
--
--
--evaluate :: Board -> Col -> Int
--evaluate b col = (getPlayersScore b col) -  (getPlayersScore b other_col)
---- case snd (won b) of
----                        Just col -> 1000000
----                        Just other_col -> -1000000
----                        Nothing ->
--                   where other_col = other col
--
--
--createLines ::Board -> [(Position, Direction)]
--createLines b = zip (zip [0..s] (repeat 0)) (repeat (0,1))++ --N && S
--                zip (zip (repeat 0) [0..s]) (repeat (1,0)) ++ --W && E
--                zip (zip [l,l-1..0] (repeat 0)) (repeat (1,1)) ++zip (zip (repeat 0) [1..l]) (repeat (1,1))++
--                zip (zip (repeat 0) [l..s]) (repeat (1,-1))++zip (zip [1..l] (repeat s)) (repeat (1,-1))
--               where s = size b
--                     l = s - (target b)
--
--createLine :: Board -> Int -> [(Position, Direction)] -> [Position]
--createLine b 0 pos     = filter (\(x,y) -> (x<=s && x>=0) && (y<=s && y>=0)) $ map fst pos
--                       where s = fromIntegral (size b)
--createLine b n (p:pos) = createLine b (n-1) (((p1+d1, p2+d2),(d1, d2)):(p:pos))
--                       where d1 = fst (snd p )
--                             d2 = snd (snd p)
--                             p1 = fst (fst p)
--                             p2 = snd (fst p)
--
--
--
--
----checkLine :: Int -> Board -> Col -> [Position] -> (Int, [Position])
----checkLine target b c pos  = foldl (\(s,p) (x,y)  ->
----                               case ((x,y),c) `elem` (pieces b) of
----                                     True -> (s+1, (x,y):p)
----                                     False -> if s >= target then (s,p) else (0, [])
----                               ) (0, []) pos
--
----checkLine :: Int -> Board -> Col -> [Position] -> [(Int, Maybe Col)]
----checkLine target b c pos  = tail $ scanl (\(s,p) (x,y)  ->  if ((x,y),c) `elem` (pieces b) then (s+1, Just c)
----                                                     else if ( ((x,y),(other c)) `elem` (pieces b) ) then (0, Just (other c))
----                                                     else (0, Nothing)
----                                       ) (0, Nothing)  pos
--
--
----checkWinnable :: Board ->  [(Int, (Position, Maybe Col))] -> Col -> Bool
----checkWinnable b posses c = (foldl(\acc (s, (p, col)) -> case col of
----                                                          Just col -> if col == c then acc else 0
----                                                          Nothing -> acc+1
----
----                                        ) 0 posses ) >= (target b)
--
--
--                                       --  trace (show first ++ "  " ++ show last ++ "  "++show next_x) False
--
--
--
--
----                                    scanl (\(a1, a2) (s, (p, col)) -> case col of
----                                                                    Just col -> if col == c then ((a1+1), a2) else (0,0)
----                                                                    Nothing -> (a1, a2+1)
----                                    ) (0, 0) posses in trace (show x) False
--
--
--                                    -- in trace (show x) x >= (target b)
--
----
----scoreRow :: Board ->  [(Int, (Position, Maybe Col))] -> Col -> Int
----scoreRow b posses c | checkWinnable b posses c == False = 0
----                    | otherwise = let x = foldl(\(a1, a2) (s, (p, col)) -> case col of
----                                                                               Just col -> if col == c  then (a1+10, a1+10 `max` a2)
----                                                                                           else (0, a2)
----                                                                               Nothing -> let new_a1 = a1+2 in if new_a1 > a2 then (new_a1, new_a1) else (new_a1,a2)
----                                                 ) (0,0) posses in trace (show x) $ snd x
--
----                                        let x =  foldl(\acc (s, (p, col)) -> case col of
----                                                                    Just col -> if col == c then (s+10):acc else (s-8):acc
----                                                                    Nothing -> 0:acc
----                                        ) [] posses
--
--                        -- where score_pos = zip (map fst posses) (map fst ((map snd) posses))
--
--
----getTotalScore :: Board -> Col -> Bool
----getTotalScore b c = if checkLines b c (target b) combinations == 1 then True else False
----                   where combinations = createLines b -- [(x, y) | x <-  map (fst) (filter ((==c).snd) (pieces b)), y <- dirs]
--
--
----checkPositions :: Board -> Col -> Int -> [(Position, Direction)] -> Int
----checkPositions _ _ _ [] = 0
----checkPositions b c target (x:xs) | checkPosition b p d c target == True = 1
----                                 | otherwise = checkPositions b c target xs --(filter (`notElem` (map (\(x,y) -> ((x,y),d)) $ createLine b p d)) xs)
----                                where p = fst x
----                                      d = snd x
--
----((0,3),Black),((1,3),Black),((4,3),Black),((4,4),White),((6,3),Black),((3,4),White
----(4,[(6,3),(4,3),(1,3),(0,3)])
--
--
----
----
----
----onBoard :: Position -> Bool
----onBoard p = if (fst p >= 0 && fst p < 6) && (snd p >= 0 && snd p < 6) then True else False
----
----
----
----
----countAdjacentPieces :: Board -> [Position] -> [Maybe Col]
----countAdjacentPieces b pos =  map (\(x,y) -> if  ((x,y),Black) `elem` (pieces b) then Just Black
----                                           else if ((x,y),White) `elem` (pieces b) then Just White
----                                           else Nothing
----                                   ) pos
----
----checkWinable :: Board -> Position -> Direction -> Col -> Int -> Bool
----checkWinable b p d c 0 = True
----checkWinable b p d c n | onBoard p == False = False
----                       | p `elem` map (fst) (filter ((==c).snd) (pieces b))  = checkWinable b next d c (n-1)
----                       | p `notElem` map (fst) (pieces b) = checkWinable b next d c (n-1)
----                       | otherwise = False
----                   where next = (fst p + fst d, snd p + snd d)
----
----
--
----
----
------checkForOpens :: Int -> Board -> [(Position, Direction)] -> Col -> Int
------checkForOpens no b posses col = foldl(\acc (p,d) -> if (checkPosition b p d  ) then acc+1 else acc) 0 posses
----
----
----
----
----
----
--
----You might use an evaluation function based on ranking each square by the values of all the rows it is contained in
--
----for each empty square on the board
----   for each row of 5 containing that square
----      add the value of that type of row to the value of the square
----   end
----end
----11*15 horizontal rows of 5
----15*11 vertical rows of 5
----11*11 / diagonal rows of 5
----11*11 \ diagonal rows of 5
----A square in the middle of the board will be contained in
----5 of those rows in each of 4 directions.
----If a square is a member of a row of 4 X's, it's a win
----If a square is a member of a row of 4 O's, it's a forced block.
----If a square is a member of two different (but overlaping) rows of 3 X's
----it's a forced win becaue playing there will create two rows of 4, and the opponent can only block one.
--
--
----checkWinnable :: Board -> [Position] -> Bool
----checkWinnable b pos = foldl (\x acc -> ) 0 pos
----                    where t = (target b)
----
----scoreLines :: Col -> Board -> [Position]-> [Int]
----scoreLines b p =  scanr (\x acc -> if ) True line
----                  where line = countAdjacentPieces b p
--
--
----                      s = fromIntegral (size b)
----                      combinations = createLines b
----                let own_score   = (getScore b col p1 2)*20 + (getScore b col p1 3)*30
----                      thier_score = (getScore b (other col) p2 1)*10 + (getScore b (other col) p2 2)*20 + (getScore b (other col) p2 3)*30
----                  in (own_score-thier_score)
----                where
--
---- if 4, check 3, check 2, or check for empty
----((checkLines b col 2 combinations)*100+ (checkLines b col 3 combinations)*1000) -- + (checkLines b col 4 combinations)*(-100))
--                ---((checkLines b (other col) 2 combinations)*1000+ (checkLines b (other col) 3 combinations)*1000)
module Board where

import Data.List
import Debug.Trace
import Data.Maybe
import Data.List.Split

dirs = [(0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)]
--          0             1             2           3           4           5           6           7
data Cell = B | W | E
    deriving (Show, Eq)

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
                     won :: (Bool, Maybe Col)
                   }
  deriving Show


data Flags = Flags { hardAi :: Bool, -- HardAi           -h
                     whiteP :: Bool -- Player is White  -w
                     }
 deriving Show




-- Default board is 6x6, target is 3 in a row, no initial pieces

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board,
                     turn :: Col,
                     hints :: Bool,
                     game_over :: Bool,
                     h_player :: Col,
                     ai_level :: Int,
                     last_move :: Maybe (Position, Col),
                     previous_board :: Board

                     }
 deriving Show

initBoard = Board 6 3 [] (False, Nothing)


makeWorld :: Flags -> IO World
makeWorld (Flags h w) = do return(initWorld (Flags h w) initBoard)


initWorld :: Flags -> Board -> World
initWorld (Flags h w) board = World  board
                                     Black
                                     False
                                     False
                                     (if w then White else Black)
                                     (if h then 3 else 1)
                                     Nothing
                                     board



-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)

makeMove :: Board -> Col -> Position -> Maybe Board
makeMove b c p
            | fst (won b) = Nothing
            | fst p  < s && snd p < s  && fst p  > -1 && snd p > -1 =
                                            case elem p (map fst (pieces b)) of
                                                True -> Nothing
                                                False ->case checkWon (b {pieces =  ((p, c):pieces b)}) c of
                                                            True -> trace (show score) Just (b {pieces =  ((p, c):pieces b), won=(True, Just c)})
                                                            False ->trace (show score) Just (b {pieces =  ((p, c):pieces b)})
            | otherwise = Nothing

            where s = (fromIntegral $ size b)+1
                  score = evaluate b c


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
checkLines b c target (x:xs) | maximum (map fst (checkLine b c (createLine b s (x:[])))) == target = 1
                             | otherwise = checkLines b c target xs
                             where s = (size b)

checkLine :: Board -> Col -> [Position] -> [(Int, (Position, Maybe Col))]
checkLine b c pos  = tail $ scanl (\(s,(p,col)) (x,y)  ->  if ((x,y),c) `elem` (pieces b) then (s+1, ((x,y), Just c))
                                                                  else if ( ((x,y),(other c)) `elem` (pieces b) ) then (0, ((x,y), Just (other c)))
                                                                  else (0, ((x,y), Nothing))
                               ) (0, ((pos!!0), Nothing))  pos

sumList ::[(Int, (Position, Maybe Col))] -> Col -> [(Int, Bool)]
sumList posses c =  tail $ scanl (\(a1, a2) (s,(p,col))  -> case col of
                                                Just col -> if col == c then (a1+1, False) else (0, False)
                                                Nothing -> (a1+1, a2)
                      ) (0, True) posses

checkOpening :: Col-> (Maybe Col, Maybe Col) -> Int
checkOpening col cols |cols == (Just col, Just col) = 0
                      |Just col == fst cols || Just col == snd cols=  1
                      |otherwise =  2



checkPartialRow ::  Col -> [Int] -> Maybe Col -> Maybe Col -> Bool
checkPartialRow = undefined


checkWinnable :: Board -> [(Int, (Position, Maybe Col))] -> Col -> Bool
checkWinnable b (x:xs) c  | length (x:xs) == s = let    next_x = take (target b) (x:xs)
                                                        last = snd (snd ((x:xs) !! ((length next_x)))) in
                                                    if max == [] then trace ("Here10  False"++ show next_x ++ "  "++show prev ++ "  "++show last) False

                                                    else if maximum max /= (target b)
                                                        then trace ("Here11 Next "++ show next_x ++ "  "++show prev ++ "  "++show last) checkWinnable b xs c
                                                    else if empty == False && checkOpening c (Nothing, last) <= 1
                                                        then trace ("Here13 True "++ show next_x ++ "  "++show prev ++ "  "++show last) False
                                                    else
                                                         trace ("Here14 Next "++ show next_x ++ "  "++show prev ++ "  "++show last ++" " ++show (next_x) ) checkWinnable b xs c

                          | length xs == t =  if max == [] then trace ("Here1 False"++ show next_x ++ "  "++show prev ++ "  "++show max) False
                                             else if maximum max /= (target b)
                                                 then trace ("Here2 False "++ show next_x ++ "  "++show prev ++ "  "++show max ++ " " ++show xs)False

                                             else if empty == False &&  checkOpening c (prev, Nothing) == 0
                                                 then trace ("Here3 True "++ show next_x ++ "  "++show prev ++ "  "++show max++ " " ++show xs) False
                                             else
                                                 trace ("Here5 False"++ show next_x ++ "  "++show prev ++ "  "++show max++ " " ++show xs) True

                          | otherwise      = let last = snd (snd ((xs) !! ((length next_x)))) in
                                             if  max == [] then trace ("Here5.5 F "++ show next_x ++ "  "++show prev ++ "  "++show max ++ "  "++show last ++ "  ") False
                                             else if maximum max /= (target b)
                                                   then trace ("Here6 NEXT"++ show next_x ++ "  "++show prev ++ "  "++show max ++ "  "++show last ++ "  ") checkWinnable b xs c

                                             else if checkOpening c (prev, last) == 2
                                                   then trace ("Here7 True"++ show next_x ++ "  "++show prev ++ "  "++show max ++ "  "++show last ++ "  ")  True
                                             else
                                                  trace ("Here8 Next"++ show next_x ++ "  "++show prev ++ "  "++show max ++ "  "++show last ++ "  ")   checkWinnable b xs c
                          where next_x = take (target b) (xs)
                                prev = snd (snd x)
                                max  =  map fst $ sumList next_x c
                                empty = False `elem` (map snd (sumList next_x c))
                                t = (target b)
                                s = (size b)+1

getScore :: Board -> Col -> Int -> [(Position, Direction)] -> Int
getScore b c target lines = let yy = foldr(\x acc -> let l = reverse $ [(6,6),(5,6),(4,6),(3,6),(2,6),(1,6),(0,6)]
                                                         l' = checkLine b c l
                                                         p = checkWinnable b l' c in
                                                               if p then True-- acc + (sum $ map fst $ filter (\(s,(p,col)) -> col == Just c)l')
                                                               else False--acc
                                   ) False lines in trace (show yy ++" " ++show c) 10

                            where s = (size b)


getPlayersScore :: Board -> Col -> Int
getPlayersScore b col = let four  = (getScore b col 4 coms)*100
                            three = (getScore b col 3 coms)*50
                            two   = (getScore b col 2 coms)*25
                            in (two + three + four)
                        where coms = createLines b



evaluate :: Board -> Col -> Int
evaluate b col =  (getPlayersScore b col) -  (getPlayersScore b (other col))


createLines ::Board -> [(Position, Direction)]
createLines b = zip (zip [0..s] (repeat 0)) (repeat (0,1))++ --N && S
                zip (zip (repeat 0) [0..s]) (repeat (1,0)) ++ --W && E
                zip (zip [l,l-1..0] (repeat 0)) (repeat (1,1)) ++zip (zip (repeat 0) [1..l]) (repeat (1,1))++
                zip (zip (repeat 0) [l..s]) (repeat (1,-1))++zip (zip [1..l] (repeat s)) (repeat (1,-1))
               where s = size b
                     l = s - (target b)

createLine :: Board -> Int -> [(Position, Direction)] -> [Position]
createLine b 0 pos     = filter (\(x,y) -> (x<=s && x>=0) && (y<=s && y>=0)) $ map fst pos
                       where s = fromIntegral (size b)
createLine b n (p:pos) = createLine b (n-1) (((p1+d1, p2+d2),(d1, d2)):(p:pos))
                       where d1 = fst (snd p )
                             d2 = snd (snd p)
                             p1 = fst (fst p)
                             p2 = snd (fst p)




--checkLine :: Int -> Board -> Col -> [Position] -> (Int, [Position])
--checkLine target b c pos  = foldl (\(s,p) (x,y)  ->
--                               case ((x,y),c) `elem` (pieces b) of
--                                     True -> (s+1, (x,y):p)
--                                     False -> if s >= target then (s,p) else (0, [])
--                               ) (0, []) pos

--checkLine :: Int -> Board -> Col -> [Position] -> [(Int, Maybe Col)]
--checkLine target b c pos  = tail $ scanl (\(s,p) (x,y)  ->  if ((x,y),c) `elem` (pieces b) then (s+1, Just c)
--                                                     else if ( ((x,y),(other c)) `elem` (pieces b) ) then (0, Just (other c))
--                                                     else (0, Nothing)
--                                       ) (0, Nothing)  pos


--checkWinnable :: Board ->  [(Int, (Position, Maybe Col))] -> Col -> Bool
--checkWinnable b posses c = (foldl(\acc (s, (p, col)) -> case col of
--                                                          Just col -> if col == c then acc else 0
--                                                          Nothing -> acc+1
--
--                                        ) 0 posses ) >= (target b)


                                       --  trace (show first ++ "  " ++ show last ++ "  "++show next_x) False




--                                    scanl (\(a1, a2) (s, (p, col)) -> case col of
--                                                                    Just col -> if col == c then ((a1+1), a2) else (0,0)
--                                                                    Nothing -> (a1, a2+1)
--                                    ) (0, 0) posses in trace (show x) False


                                    -- in trace (show x) x >= (target b)

--
--scoreRow :: Board ->  [(Int, (Position, Maybe Col))] -> Col -> Int
--scoreRow b posses c | checkWinnable b posses c == False = 0
--                    | otherwise = let x = foldl(\(a1, a2) (s, (p, col)) -> case col of
--                                                                               Just col -> if col == c  then (a1+10, a1+10 `max` a2)
--                                                                                           else (0, a2)
--                                                                               Nothing -> let new_a1 = a1+2 in if new_a1 > a2 then (new_a1, new_a1) else (new_a1,a2)
--                                                 ) (0,0) posses in trace (show x) $ snd x

--                                        let x =  foldl(\acc (s, (p, col)) -> case col of
--                                                                    Just col -> if col == c then (s+10):acc else (s-8):acc
--                                                                    Nothing -> 0:acc
--                                        ) [] posses

                        -- where score_pos = zip (map fst posses) (map fst ((map snd) posses))


--getTotalScore :: Board -> Col -> Bool
--getTotalScore b c = if checkLines b c (target b) combinations == 1 then True else False
--                   where combinations = createLines b -- [(x, y) | x <-  map (fst) (filter ((==c).snd) (pieces b)), y <- dirs]


--checkPositions :: Board -> Col -> Int -> [(Position, Direction)] -> Int
--checkPositions _ _ _ [] = 0
--checkPositions b c target (x:xs) | checkPosition b p d c target == True = 1
--                                 | otherwise = checkPositions b c target xs --(filter (`notElem` (map (\(x,y) -> ((x,y),d)) $ createLine b p d)) xs)
--                                where p = fst x
--                                      d = snd x

--((0,3),Black),((1,3),Black),((4,3),Black),((4,4),White),((6,3),Black),((3,4),White
--(4,[(6,3),(4,3),(1,3),(0,3)])


--
--
--
--onBoard :: Position -> Bool
--onBoard p = if (fst p >= 0 && fst p < 6) && (snd p >= 0 && snd p < 6) then True else False
--
--
--
--
--countAdjacentPieces :: Board -> [Position] -> [Maybe Col]
--countAdjacentPieces b pos =  map (\(x,y) -> if  ((x,y),Black) `elem` (pieces b) then Just Black
--                                           else if ((x,y),White) `elem` (pieces b) then Just White
--                                           else Nothing
--                                   ) pos
--
--checkWinable :: Board -> Position -> Direction -> Col -> Int -> Bool
--checkWinable b p d c 0 = True
--checkWinable b p d c n | onBoard p == False = False
--                       | p `elem` map (fst) (filter ((==c).snd) (pieces b))  = checkWinable b next d c (n-1)
--                       | p `notElem` map (fst) (pieces b) = checkWinable b next d c (n-1)
--                       | otherwise = False
--                   where next = (fst p + fst d, snd p + snd d)
--
--

--
--
----checkForOpens :: Int -> Board -> [(Position, Direction)] -> Col -> Int
----checkForOpens no b posses col = foldl(\acc (p,d) -> if (checkPosition b p d  ) then acc+1 else acc) 0 posses
--
--
--
--
--
--

--You might use an evaluation function based on ranking each square by the values of all the rows it is contained in

--for each empty square on the board
--   for each row of 5 containing that square
--      add the value of that type of row to the value of the square
--   end
--end
--11*15 horizontal rows of 5
--15*11 vertical rows of 5
--11*11 / diagonal rows of 5
--11*11 \ diagonal rows of 5
--A square in the middle of the board will be contained in
--5 of those rows in each of 4 directions.
--If a square is a member of a row of 4 X's, it's a win
--If a square is a member of a row of 4 O's, it's a forced block.
--If a square is a member of two different (but overlaping) rows of 3 X's
--it's a forced win becaue playing there will create two rows of 4, and the opponent can only block one.


--checkWinnable :: Board -> [Position] -> Bool
--checkWinnable b pos = foldl (\x acc -> ) 0 pos
--                    where t = (target b)
--
--scoreLines :: Col -> Board -> [Position]-> [Int]
--scoreLines b p =  scanr (\x acc -> if ) True line
--                  where line = countAdjacentPieces b p


--                      s = fromIntegral (size b)
--                      combinations = createLines b
--                let own_score   = (getScore b col p1 2)*20 + (getScore b col p1 3)*30
--                      thier_score = (getScore b (other col) p2 1)*10 + (getScore b (other col) p2 2)*20 + (getScore b (other col) p2 3)*30
--                  in (own_score-thier_score)
--                where

-- if 4, check 3, check 2, or check for empty
--((checkLines b col 2 combinations)*100+ (checkLines b col 3 combinations)*1000) -- + (checkLines b col 4 combinations)*(-100))
                ---((checkLines b (other col) 2 combinations)*1000+ (checkLines b (other col) 3 combinations)*1000)
