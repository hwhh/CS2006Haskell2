module AI where

import Board
import Debug.Trace
import qualified Data.List as L
import Data.Maybe
import Data.Function
import qualified Data.Set as Set
import System.Exit

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }
    deriving Show


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

minimax :: (Int,Int) -> GameTree -> Int
minimax (0,level) gt = (evaluate level (game_board gt) (game_turn gt))
minimax (d,level) gt = -minimum (map (minimax (d-1, level+1)) (map snd (next_moves gt)))

minimax_ab :: (Int,Int) -> Int -> Int  -> GameTree -> Int
minimax_ab (0,level) a b gt = a `max`(evaluate level (game_board gt) (game_turn gt)) `min` b
minimax_ab (d,level) a b gt = prune (d, level) a b (map snd (next_moves gt))
        where prune (d, level) a b  [] = a
              prune (d, level) a b (t:ts)
                        | a' == b = a'
                        | otherwise = prune (d, level) a' b ts
                     where a' =  -(minimax_ab (d-1, level+1) (-b) (-a) t)

-- | MiniMax algorithm
minimax' :: (Int,Int) ->  Bool-> GameTree -> Int
minimax' (0,level) max gt = case max of -- When depth of 0 evalute board
                           True -> (evaluate level (game_board gt)  $ other(game_turn gt))
                           False -> (evaluate level (game_board gt)  (game_turn gt))
minimax' (d,level) True gt = case length (next_moves gt) == 0 of
                           True -> evaluate level (game_board gt) (other(game_turn gt))
                           False -> minimum $ map (minimax'  (d-1, level+1) False) (map snd (next_moves gt))
minimax' (d,level) False gt = case length (next_moves gt) == 0 of
                           True ->  evaluate level (game_board gt)  (game_turn gt)
                           False -> maximum $ map (minimax' (d-1, level+1) True) (map snd (next_moves gt))



getBestMove :: Int -- ^ Maximum search depth
               -> World
               -> GameTree -- ^ Initial game tree
               -> (Int, Position)
getBestMove d w gt | d==3 = maximum $ let x = zip (map (negate . minimax_ab (2,0) (-100000) 100000 . snd) (next_moves gt)) (map fst (next_moves gt)) in trace (show x) x
                   | otherwise =  maximum $ zip (map (minimax' (3,0) True . snd) (next_moves gt)) (map fst (next_moves gt))




-- Update the world state after some time has passed
updateWorld :: Float --  time since last update (you can ignore this)1
            -> World --  current world state
            -> IO World
updateWorld t w | (turn w == h_player w || (pVp w)) = return $ w
                | otherwise = let move = getBestMove (ai_level w) w $ buildTree generateMoves b col in
                                  case makeMove (board w) (col) (snd move) of
                                       Just new_board -> case fst $ won new_board of
                                                   True ->  return $ w {board = new_board, turn = other col}
                                                   False -> return $ w {board = new_board, turn = other col}
                                                   --True ->  return $ w {board = (new_board{score = (evaluate 0 new_board col)}), turn = other col}
                                                   --False -> return $ w {board = (new_board{score = (evaluate 0 new_board col)}), turn = other col}
                                       Nothing -> return $ w
                where b = board w
                      col = turn w
                      pd = (board w)



-- |Generates the moves
generateMoves :: Board -> Col -> [Position]
generateMoves b c |length (pieces b) == (0)  = let centre = ceiling (fromIntegral s / 2) in [(centre, centre)]
                  |length winning     > 0  =  winning
                  |length close       > 0  =  close
                  |otherwise               =  all
                  where all = getAllMoves b
                        winning = getWinningMoves b c all
                        close  =  getCloseMoves b all (1,1)
                        s = size b

distanceFrom :: Position -> Position -> Position -> Bool
distanceFrom p1 p2 dis | (abs (fst p1 - fst p2)) <= fst dis && (abs (snd p1 - snd p2)) <= snd dis = True
                       | otherwise = False

getCloseMoves :: Board -> [Position] -> (Int, Int) ->[Position]
getCloseMoves b all_moves (x,y) = Set.toList . Set.fromList $ foldl(\acc (x1,y1) -> acc ++ foldl(\ acc2 (x2,y2) ->
                                                    case distanceFrom (x1,y1) (x2,y2) (x, y) of
                                                        True -> (x1,y1):acc2
                                                        False -> acc2
                                                )[] occupied_positions
                    ) [] all_moves
                where occupied_positions = filter (`elem` (map (\((x,y), c) -> (x,y)) $ pieces b)) [(x,y) | x <-[0.. size b], y <-[0..  size b]]
                      current_score = evaluate

-- |Gets all the free moves
getAllMoves :: Board -> [Position]
getAllMoves b = filter (`notElem` (map (\((x,y), c) -> (x,y)) $ pieces b)) [(x,y) | x <-[0.. size b], y <-[0..  size b]]

-- |Gets winning moves; not fully functional
getWinningMoves :: Board -> Col -> [Position] ->[Position]
getWinningMoves b c all_moves = foldr(\(x,y) acc-> case makeMove b c (x,y) of
                                                        Just b1 -> if fst (won b1)
                                                                        then (x,y):acc
                                                                   else
                                                                        case makeMove b (other c) (x,y) of
                                                                             Just b2 -> if fst (won b2)  then (x,y):acc else acc
                                                                             Nothing -> acc
                                                        Nothing -> acc

                                       ) [] all_moves






-- |Gets moves with good scors
getBestMoves :: Board -> Col -> [Position] ->[Position]
getBestMoves b c moves = let scored =  foldl(\acc (x,y) -> case makeMove b c (x,y) of
                                                  Just b -> ((evaluate 0 b c), (x,y)):acc
                                                  Nothing -> acc) [] moves
                         in map snd $ reverse (L.sortBy (compare `on` fst) scored)

