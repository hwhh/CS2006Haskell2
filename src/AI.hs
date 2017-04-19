module AI where

import Board
import Debug.Trace
import Data.List
import Data.Maybe
import Data.Function

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }
    deriving Show

-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree

buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                             -- successful, make move and build tree from
                             -- here for opposite player



-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.


minimax :: Int -> Bool-> GameTree -> Int
minimax 0 max gt = case max of
                    True -> evaluate (game_board gt) $ other (game_turn gt)   --maximum $ foldr(\pos acc -> evaluate (game_board gt)  (game_turn gt):acc) []  (next_moves gt)--foldr(\pos bv ->  bv `max` evaluate (game_board gt) (game_turn gt))         ((minBound :: Int)+1)  (next_moves gt)--maximum evaluate (game_board gt) $ other (game_turn gt) -- in trace (show (game_board gt) ++ " "++ show (game_turn gt) ++ " "++ show x ) x
                    False -> evaluate (game_board gt) (game_turn gt) -- minimum $ foldr(\pos acc ->  evaluate (game_board gt) (other $ game_turn gt):acc) [] (next_moves gt)-- in trace (show (game_board gt) ++ " "++ show (game_turn gt) ++ " "++ show x ) x
minimax d True gt =  if length (next_moves gt) == 0
                         then (maxBound::Int)
                     else maximum $ foldl (\acc moves -> (minimax (d-1) False (snd moves)):acc) [] (next_moves gt)
minimax d False gt = if length (next_moves gt) == 0
                         then (minBound::Int)
                     else minimum $ foldl (\acc moves -> (minimax (d-1) True (snd moves)):acc) [] (next_moves gt)


minimax_ab :: Int -> Int -> Int ->GameTree -> Int
minimax_ab 0 a b  gt = evaluate (game_board gt) (other (game_turn gt))
minimax_ab d a b  gt | length (next_moves gt) == 0  = (maxBound :: Int)
                     | otherwise = cmx a (map snd (next_moves gt))
                    where cmx a []  = a
                          cmx a (x:xs) | a'>=b     = a'
                                      | otherwise = cmx a' xs
                                     where a' = -(minimax_ab (d-1) (-b) (-a)  x)

getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove n gt = snd $ maximum $ let x =  zip (map (minimax 2 True .snd) (next_moves gt)) (map fst (next_moves gt)) in trace (show x) x
                            where alpha = (minBound :: Int)+1
                                  beta  = (maxBound :: Int)-1



-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)1
            -> World -- ^ current world state
            -> IO World
updateWorld t w   | turn w == h_player w   = return $ w --
                  | otherwise = let move = getBestMove 3 $ buildTree generateMoves b col
                                          in case makeMove (board w) (other $ h_player w) move of
                                                 Just new_board -> case fst $ won new_board of
                                                       True -> return $ w {board = new_board, turn = other col}
                                                       False -> return $ w {board = new_board, turn = other col}
                                                 Nothing -> return $ w
                   where b = board w
                         col = turn w
                         pd = (board w)


{- Hint: 'updateWorld' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateWorld should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 At first, it is reasonable for this to be a random move!

 If both players are human players, the simple version above will suffice,
 since it does nothing.

 In a complete implementation, 'updateWorld' should also check if either
 player has won and display a message if so.
-}

generateMoves :: Board -> Col -> [Position]
generateMoves b c |length (pieces b) == 0  = [(3,3)]
                  |length winning     > 0  =  winning
                  |length good        > 0  =  good
                  |otherwise               = all
                   where all = getAllMoves b
                         winning = [] -- getWinningMoves b c all
                         good = []--getBestMoves b c all

getAllMoves :: Board -> [Position]
getAllMoves b = filter (`notElem` (map (\((x,y), c) -> (x,y)) $ pieces b)) [(x,y) | x <-[0..fromIntegral $ size b], y <-[0..fromIntegral $ size b]]

getWinningMoves :: Board -> Col -> [Position] ->[Position]
getWinningMoves b c all_moves = foldr(\(x,y) acc-> case makeMove b c (x,y) of
                                                            Just new_board -> if fst (won new_board) then (x,y):acc else acc
                                                            Nothing -> acc
                                       ) [] all_moves


getBestMoves :: Board -> Col -> [Position] ->[Position]
getBestMoves b c all_moves = let a = foldr(\(x,y) acc-> case makeMove b c (x,y) of
                                                                  Just new_board -> let score = evaluate new_board c in
                                                                                        if score > 0 then (score, (x,y)):acc else acc
                                                                  Nothing -> acc
                                        ) [] all_moves in map snd $ take 10 $ reverse (sortBy (compare `on` fst) a)




