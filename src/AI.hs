module AI where

import Board
import Debug.Trace
import Data.List
import Data.Maybe


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

minimax :: Int -> GameTree -> Int
minimax 0 (GameTree b col _)    = evaluate b col
minimax d (GameTree b col [])   = evaluate b col
minimax d (GameTree b White xs) = minimum $ map (minimax (d - 1)) (map snd (xs))
minimax d (GameTree b Black xs) = maximum $ map (minimax (d - 1)) (map snd (xs))

--minimaxa :: Int -> Int -> Int -> GameTree -> Int
--minimaxa 0 a b (GameTree board  col _)    = evaluate board col
--minimaxa d a b (GameTree board col [])    = evaluate board col
--minimaxa d a b (GameTree board White xs) = minimum $ map (minimax (d - 1)) -- function ommits tree branches (map snd (xs))
--minimaxa d a b (GameTree board Black xs) = maximum $ map (minimax (d - 1)) (map snd (xs))


minimax_ab :: Int -> Int -> Int -> GameTree -> Int
minimax_ab 0 a b (GameTree board col _)   = a `max` evaluate board col `min` b
minimax_ab d a b (GameTree board col [])  = a `max` evaluate board col `min` b
minimax_ab d a b (GameTree board col xs) = cmx a (map snd (xs))
    where cmx a []  = a
          cmx a (x:xs) | a'>=b     = a'
                       | otherwise = cmx a' xs
                       where a' = -(minimax_ab (d-1) (-b) (-a) x)

getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove n gt = --snd $ maximum $ let x = zip (map (minimax (n-1).snd) (next_moves gt)) (map fst (next_moves gt)) in trace(show x)  x
                   snd $ maximum $ let x = zip (map (minimax_ab n alpha beta.snd) (next_moves gt)) (map fst (next_moves gt)) in trace(show x)  x
                            where alpha = minBound :: Int
                                  beta  = maxBound :: Int



-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
updateWorld t w | turn w == White = w
                | turn w == Black = let move = getBestMove 3 $ buildTree generateMoves b col
                                        in case makeMove (board w) Black move of
                                               Just new_board -> case fst $ won new_board of
                                                     True -> w {board = new_board, turn = other col}
                                                     False -> w {board = new_board, turn = other col}
                                               Nothing -> w
                 where b = board w
                       col = turn w



generateMoves :: Board -> Col -> [(Position)]
generateMoves b c = filter (`notElem` (map (\((x,y), c) -> (x,y)) $ pieces b)) [(x,y) | x <-[0.0..fromIntegral $ size b -1], y <-[0.0..fromIntegral $ size b -1]]



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


--getBestMove 2 gt = snd $ maximum $ let x = zip (map ((minimax (2)).snd) (next_moves gt)) (map fst (next_moves gt)) in trace(show x) x


--
--foldl1(\(f,s) bst ->  let  next_tree = find (\x ->fst x == (f,s)) $ next_moves g
--                                             score = minimax n (snd $ fromJust next_tree)
--                                         in if score > x then let x = score in trace (show $ bst) bst
--                                            else trace ("here "++ (show $ (f,s))) (f,s)
--
--                       )
--
--
--


--updateWorld t w | turn w == White = w
--                | turn w == Black = let move = generateMoves (board w) Black !! 0
--                                        col = turn w
--                                    in case makeMove (board w) Black move of
--                                               Just new_board -> case fst $ won new_board of
--                                                     True -> w {board = new_board, turn = other col}
--                                                     False -> w {board = new_board, turn = other col}
--                                               Nothing -> w

--minimax :: Int -> GameTree -> Int
--minimax 0 (GameTree b col _)    = evaluate b col --let x = evaluate b col in trace (show x) x
--minimax d (GameTree b col [])   = evaluate b col  --let x = evaluate b col in trace (show x) x
--minimax d (GameTree b Black xs) = maximum $ map (minimax (d-1)) (map snd xs)
--minimax d (GameTree b White xs) = minimum $ map (minimax (d-1)) (map snd xs)

--foldl1(\(f,s) (p1, p2) ->
--                       case makeMove (game_board gt) (game_turn gt) (f,s) of
--                             Just new_board -> let new_val =  minimax (game_turn gt) n new_board  in
--                                                 if new_val > x then
--                                                    let x = new_val in trace (show x ++ " " ++ show new_val ++" " ++ show (p1, p2)) (p1, p2)
--                                                 else trace ("HERE") (f, s)
--                             Nothing -> (f, s)
--                         )  $  generateMoves (game_board gt) (game_turn gt)
--                where x = minBound :: Int

-- foldl1(\((p1,p2),g) t -> if minimax n g > x then (p1,p2) else x) map snd $ next_moves gt

               --

--scoreMoves :: Int -> Int -> [(Position, GameTree)] -> Position -> Position
--scoreMoves d current [] p = p
--scoreMoves d current (x:xs) p
--                              | new_val > current         = trace ("Here 0" ++ show  (fst x) ++ " "++ show current++" "++ (show new_val)) scoreMoves d new_val xs (fst x) -- trace (show y++" 1. "++show current) --| True = let gt = snd x in trace (show (game_board gt) ++ " "++ show new_val) scoreMoves d y xs (fst x)
--                              | otherwise                 = trace ("Here 1" ++ show  (fst x) ++ " "++ show current++" "++ (show new_val)) scoreMoves d current xs p -- trace (show y++" 2. "++show current) scoreMoves d current xs p
--                       where new_val = minimax (d-1) (snd x)


-- | True = trace (show new_val) scoreMoves d y xs (fst x)
--
--
--fst $ foldl1(\(p, g) bst->  let new_val = minimax (n-1) g in
--                                                          if trace (show x) new_val > x then (bst)
--                                                           else (p, g)
--                                       ) (next_moves gt)
--                       where x = minBound :: Int

--minimax :: Col -> Int -> Board -> Int
--minimax col 0 b    = evaluate b col
--minimax White d b  = maximum $ map (minimax Black (d-1) b) b
--minimax Black d b  = minimum $ map (minimax White (d-1) b) b


--minimax :: Int -> GameTree -> Int
--minimax 0 (GameTree b col _)    = let x= evaluate b col in if x == 1000 then trace (show b) x else x --let x = evaluate b col in trace (show x) x
----minimax 0 (GameTree b col _)    = evaluate b col --let x= evaluate b col in if x == 1 then trace (show x) x else x --let x = evaluate b col in trace (show x) x
--minimax d (GameTree b col [])   = evaluate b col --let x= evaluate b col in if x == 1 then trace (show x) x else x --let x = evaluate b col in trace ("hello" ++show (b)) x  --let x = evaluate b col in trace (show x) x
--minimax d (GameTree b White xs) = let x = minimum $ map (minimax (d-1)) (map snd xs) in trace (" White " ++ show x) x
--minimax d (GameTree b Black xs) = let x = maximum $ map (minimax (d-1)) (map snd xs) in trace (" Black " ++ show x) x

