module Board where

import Data.List
import Debug.Trace
import Data.Maybe

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
                     last_move :: Maybe (Position, Col)

                     }
 deriving Show

initBoard = Board 6 4 [] (False, Nothing)


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



-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)

makeMove :: Board -> Col -> Position -> Maybe Board
makeMove b c p
            | fst (won b) = Nothing
            | fst p  < s && snd p < s  && fst p  > -1 && snd p > -1 =
                                            case elem p (map fst (pieces b)) of
                                                True -> Nothing
                                                False ->case checkWon (b {pieces =  ((p, c):pieces b)}) c of
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
checkLine target b c pos  = foldr (\(x,y) acc ->
                               case ((x,y),c) `elem` (pieces b) of
                                     True -> acc+1
                                     False -> if acc >= target then acc else 0
                               ) 0 pos

--checkPositions :: Board -> Col -> Int -> [(Position, Direction)] -> Int
--checkPositions _ _ _ [] = 0
--checkPositions b c target (x:xs) | checkPosition b p d c target == True = 1
--                                 | otherwise = checkPositions b c target xs --(filter (`notElem` (map (\(x,y) -> ((x,y),d)) $ createLine b p d)) xs)
--                                where p = fst x
--                                      d = snd x

onBoard :: Position -> Bool
onBoard p = if (fst p >= 0 && fst p < 6) && (snd p >= 0 && snd p < 6) then True else False

checkPosition :: Board -> Position -> Direction -> Col -> Int -> Bool
checkPosition b p d c 0 | p `notElem` map (fst) (pieces b) && onBoard p = True
                        | otherwise = False
checkPosition b p d c n | p `notElem` map (fst) (filter ((==c).snd) (pieces b)) = False
                        | otherwise = checkPosition b (fst p + fst d, snd p + snd d) d c (n-1)
--

--countAdjacentPieces :: Board -> [Position] -> [Maybe Col]
--countAdjacentPieces b pos =  map (\(x,y) -> if  ((x,y),Black) `elem` (pieces b) then Just Black
--                                           else if ((x,y),White) `elem` (pieces b) then Just White
--                                           else Nothing
--                                   ) pos
--
getScore:: Board -> Col -> [(Position, Direction)] -> Int -> Int
getScore b c poss len = foldl(\acc (p,d) -> if (checkPosition b p d c len) then acc+1 else acc) 0 poss


evaluate :: Board -> Col -> Int
evaluate b col  | fst (won b) = 10000000
                | otherwise =let own_score = (getScore b col p1 1)*10 + (getScore b col p1 2)*20 + (getScore b col p1 3)*30
                                 thier_score = (getScore b col p2 1)*50 + (getScore b col p2 2)*50 + (getScore b col p2 3)*50
                              in (own_score-thier_score)
                where p1 = filter (\((x,y),(d1,d2)) -> (x+d1)<=s && (x+d1) >=0 && (y+d2)<=s && (y+d2) >=0) [(x, y) | x <-  map (fst) (filter ((==col).snd) (pieces b)), y <- dirs]
                      p2 = filter (\((x,y),(d1,d2)) -> (x+d1)<=s && (x+d1) >=0 && (y+d2)<=s && (y+d2) >=0) [(x, y) | x <-  map (fst) (filter ((==(other col)).snd) (pieces b)), y <- dirs]
                      s = fromIntegral (size b)
                      combinations = createLines b

-- if 2, check 3, check 4, or check for empty
--((checkLines b col 2 combinations)*100+ (checkLines b col 3 combinations)*1000) -- + (checkLines b col 4 combinations)*(-100))
                ---((checkLines b (other col) 2 combinations)*1000+ (checkLines b (other col) 3 combinations)*1000)



createLines ::Board -> [(Position, Direction)]
createLines b = zip (zip [0..s] (repeat 0)) (repeat (0,1))++ --N && S
                zip (zip (repeat 0) [0..s]) (repeat (1,0)) ++ --W && E
                zip (zip [l,l-1..0] (repeat 0)) (repeat (1,1)) ++zip (zip (repeat 0) [1..l]) (repeat (1,1))++ -- NE && SW
                zip (zip (repeat 0) [l..s]) (repeat (1,-1))++zip (zip [1..l] (repeat s)) (repeat (1,-1))  -- NE && SWx
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
