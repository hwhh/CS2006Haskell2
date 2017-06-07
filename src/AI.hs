module AI where

import Board
import Debug.Trace
import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Set as Set

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
buildTree :: (Board -> Col -> [Position]) --  Move generator
             -> Board --  board state
             -> Col --  player to play next
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

-- | MiniMax algorithm
minimax :: Int -> Bool-> GameTree -> Int
minimax 0 max gt = case max of -- When depth of 0 evalute board
                    True ->  evaluate (game_board gt)  $ other(game_turn gt)
                    False -> evaluate (game_board gt)  (game_turn gt)
minimax d True gt = case length (next_moves gt) == 0 of
                           True -> evaluate  (game_board gt) (other(game_turn gt))
                           False -> minimum $ map (minimax  (d - 1) False) (map snd (next_moves gt))--foldr(\child x  -> x `max` (minimax (d-1) False child)) best_vale $ map snd (next_moves gt)
minimax d False gt = case length (next_moves gt) == 0 of
                           True -> evaluate  (game_board gt)  (game_turn gt)
                           False -> maximum $ map (minimax (d - 1) True) (map snd (next_moves gt))-- foldr(\child x -> x `min` (minimax (d-1) True child)) best_vale $ map snd (next_moves gt)

-- | Minimax with alpha beta pruning;  not fully fuctional
minimax_ab :: Int -> Int -> Int ->GameTree -> Int
minimax_ab 0 a b  gt = evaluate (game_board gt) (other (game_turn gt))-- If depth of 0 return evalution
minimax_ab d a b  gt | length (next_moves gt) == 0  = (maxBound :: Int)
                     | otherwise = cmx a (map snd (next_moves gt)) -- ^ Else prune the tree
                    where cmx a []  = a
                          cmx a (x:xs) | a'>=b     = a'
                                      | otherwise = cmx a' xs
                                     where a' = -(minimax_ab (d-1) (-b) (-a)  x)



--alpha_beta :: Game_tree a => Int -- ^ Minimum score maximising player is assured
--           -> Int          -- ^ Maximum score minimizing player is assured
--           -> a            -- ^ State to be evaluated
--           -> Int          -- ^ Search this deep
--           -> ([a], Int)   -- ^ (Principal variation, Score)
--alpha_beta alpha beta node depth
--    |  depth == 0 = ([node], node_value node)
--    | otherwise                      =  case children node of
--                                          (c:cs) -> (node:pvm, pvv)
--                                              where (pvm, pvv) = negaLevel ([], (minBound :: Int) + 2) alpha beta (c:cs)
--    where negaLevel prev_best@(_, old_v) prev_alpha beta' (n:nn) | old_v < beta'
--            = negaLevel best4 alpha' beta' nn
--                where best4 = case neg $ alpha_beta (-beta') (-alpha') n (depth - 1) of
--                                 value@(_, v) | (v > old_v) -> value
--                                              | otherwise -> prev_best
--                      alpha' = if old_v > prev_alpha then old_v else prev_alpha
--          negaLevel best _ _ _     = best
--          neg (m, v) = (m, -v)


--
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
--getBestMove n gt = snd $ maximum $ zip (map (minimax 2 True .snd) (next_moves gt)) (map fst (next_moves gt)) --Zip position with score
getBestMove n gt = snd $ maximum $ zip (map (minimax_ab 2 alpha beta .snd) (next_moves gt)) (map fst (next_moves gt)) --Zip position with score
                            where alpha = (minBound :: Int)+1
                                  beta  = (maxBound :: Int)-1





---- Update the world state after some time has passed
updateWorld :: Float --  time since last update (you can ignore this)1
            -> World --  current world state
            -> IO World
updateWorld t w   | turn w == h_player w || (pVp w)  = return $ w
                  | otherwise = let move = getBestMove (ai_level w) $ buildTree generateMoves b col
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

-- |Generates the moves
generateMoves :: Board -> Col -> [Position]
generateMoves b c |length (pieces b) == 0  = [(8,8)]
                  |length winning     > 0  =  winning
                  |length close > 0 && length (pieces b) <= 10  =  close
                  |otherwise               = all
                   where all = getAllMoves b
                         winning = getWinningMoves b c all
                         close = getCloseMoves b all

distanceFrom :: Position -> Position -> Position -> Bool
distanceFrom p1 p2 dis | (abs (fst p1 - fst p2)) <= fst dis && (abs (snd p1 - snd p2)) <= snd dis = True
                       | otherwise = False

getCloseMoves :: Board -> [Position] -> [Position]
getCloseMoves b all_moves = Set.toList . Set.fromList $ foldl(\acc (x1,y1) -> acc ++ foldl(\ acc2 (x2,y2) ->
                                                    case distanceFrom (x1,y1) (x2,y2) (1,1) of
                                                        True -> (x1,y1):acc2
                                                        False -> acc2
                                                )[] occupied_positions
                    ) [] all_moves
                where occupied_positions = filter (`elem` (map (\((x,y), c) -> (x,y)) $ pieces b)) [(x,y) | x <-[0.. size b], y <-[0..  size b]]




-- |Gets all the free moves
getAllMoves :: Board -> [Position]
getAllMoves b = filter (`notElem` (map (\((x,y), c) -> (x,y)) $ pieces b)) [(x,y) | x <-[0.. size b], y <-[0..  size b]]

-- |Gets winning moves; not fully functional
getWinningMoves :: Board -> Col -> [Position] ->[Position]
getWinningMoves b c all_moves = foldr(\(x,y) acc-> case makeMove b c (x,y) of
                                                            Just new_board -> if fst (won new_board) then (x,y):acc else acc
                                                            Nothing -> acc
                                       ) [] all_moves

-- |Gets moves with good scors
getBestMoves :: Board -> Col -> [Position] ->[Position]
getBestMoves b c all_moves = let a = foldr(\(x,y) acc-> case makeMove b c (x,y) of
                                                                  Just new_board -> let score = evaluate new_board c in
                                                                                        if score > 0 then (score, (x,y)):acc else acc
                                                                  Nothing -> acc
                                        ) [] all_moves in map snd $ take 10 $ reverse (sortBy (compare `on` fst) a)




















--
--
----
--getBestMove :: Int -- ^ Maximum search depth
--               -> GameTree -- ^ Initial game tree
--               -> Position
--getBestMove depth tree = fst $ getMax $ let x = foldl (\acc move -> (minPos (2) (fst move) (snd move)):acc) [] (next_moves tree) in trace (show x) x
--
---- Returns the maximum scoring position to play and associated score it would have at max depth
---- Of either the next moves evaluated (if depth == 0) or the rest of the tree evaluated by minPos
--maxPos :: Int -> Position -> GameTree -> (Position, Int)
--maxPos depth pos tree = if null (next_moves tree) then (pos,evaluate (game_board tree) (game_turn tree) )
--                        else if (depth == 0) then getMax (maxPosEvalList pos tree)
--                        else getMax $ foldl (\acc moves -> (minPos (depth-1) pos (snd moves)):acc) [] (next_moves tree)
--
---- Returns the minimum scoring move to play next
--minPos :: Int -> Position -> GameTree -> (Position, Int)
--minPos depth pos tree = if null (next_moves tree) then (pos, evaluate (game_board tree) (other $ game_turn tree) )
--                        else if (depth == 0) then getMin (minPosEvalList pos tree)
--                        else getMin $ foldl (\acc moves -> (maxPos (depth-1) pos (snd moves)):acc) [] (next_moves tree)
--
---- Returns the largest scoring position to play from a list of moves
--getMax :: [(Position, Int)] -> (Position, Int)
--getMax (x:xs) = foldl (\x acc -> if snd x > snd acc then x else acc) x xs
--
---- Returns the smallest scoring position to play from a list of moves
--getMin :: [(Position, Int)] -> (Position, Int)
--getMin (x:xs) = foldl (\acc x -> if snd x < snd acc then x else acc) x xs
--
---- Evaluates and returns each of the next moves that can be made and the associated evaluation value
--minPosEvalList :: Position -> GameTree -> [(Position, Int)]
--minPosEvalList pos tree = foldl (\acc moves -> (pos, evaluate (game_board tree) (other $ game_turn tree) ):acc) [] (next_moves tree)
--
---- Evaluates and returns each of the next moves that can be made and the associated evaluation value
--maxPosEvalList :: Position -> GameTree -> [(Position, Int)]
--maxPosEvalList pos tree = foldl (\acc moves -> (pos, evaluate (game_board tree) (game_turn tree) ):acc) [] (next_moves tree)




-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
----
--minimax :: Int -> GameTree -> Int
--minimax depth gt
--  | depth <= 0         = evaluate (game_board gt) (game_turn gt)
--  | depth `mod` 2 == 0 = let moves = map (minimax (depth - 1)) (map snd (next_moves gt))
--                         in if moves == []
--                              then evaluate  (game_board gt) (game_turn gt)
--                              else minimum $ map (minimax (depth - 1)) (map snd (next_moves gt))
--  | depth `mod` 2 == 1 = let moves = map (minimax (depth - 1)) (map snd (next_moves gt))
--                         in if moves == []
--                              then evaluate  (game_board gt) (game_turn gt)
--                              else maximum $ map (minimax (depth - 1)) (map snd (next_moves gt))
--
--getBestMove :: Int -> GameTree -> Position
--getBestMove depth tree = snd $ maximum $ let x = zip (map ((minimax (2)).snd) (next_moves tree)) (map fst (next_moves tree)) in trace (show x) x
