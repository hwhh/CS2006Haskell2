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

minimax :: Int -> Bool-> GameTree -> Int
minimax 0 _ gt = evaluate (game_board gt) (other (game_turn gt))
minimax d True gt = if length (next_moves gt) == 0 then maxBound :: Int else foldr(\child x  -> x `max` (minimax (d-1) False child)) best_vale $ map snd (next_moves gt)
                   where best_vale = (minBound :: Int)+1
minimax d False gt =if length (next_moves gt) == 0 then minBound :: Int else foldr(\child x -> x `min` (minimax (d-1) True child)) best_vale $ map snd (next_moves gt)
                   where best_vale = (maxBound :: Int)-1


minimax_ab :: Int -> Int -> Int-> GameTree -> Int
minimax_ab 0 a b gt = evaluate (game_board gt) (other (game_turn gt))
minimax_ab d a b gt = cmx a (map snd (next_moves gt))
                   where cmx a []  = a
                         cmx a (x:xs) | a'>=b     = a'
                                      | otherwise = cmx a' xs
                                     where a' = (minimax_ab (d-1) (-b) (-a) x)

getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove n gt = snd $ maximum $ let x =  zip (map (minimax 2 True . snd) (next_moves gt)) (map fst (next_moves gt)) in trace (show x) x
                   --snd $ maximum $ zip (map (minimax_ab 3 alpha beta .snd) (next_moves gt)) (map fst (next_moves gt))
                            where alpha = (minBound :: Int)+1
                                  beta  = (maxBound :: Int)-1



-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
updateWorld t w | turn w == White = w{last_move=  Just lm}
                | turn w == Black = let move = getBestMove 3 $ buildTree generateMoves b col
                                        in case makeMove (board w) Black move of
                                               Just new_board -> case fst $ won new_board of
                                                     True -> w {board = new_board, turn = other col, last_move= Just lm}
                                                     False -> w {board = new_board, turn = other col, last_move= Just lm}
                                               Nothing -> w
                 where b = board w
                       col = turn w
                       lm = pieces b !! 0


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

generateMoves :: Board -> Col -> [(Position)]
generateMoves b c =
                    filter (\(x,y) -> (x,y) `notElem` (map (\((x,y), c) -> (x,y)) $ pieces b)) [(x,y) | x <-[0.0..fromIntegral $ size b -1], y <-[0.0..fromIntegral $ size b -1]]

-- | length (pieces b) == 0 = [(2.0,2.0)]
--                  | otherwise = concat $ getAdjacentMoves all_poss $ map fst $ filter ((==c).snd) (pieces b)
--                   where all_poss = filter (`notElem` (map (\((x,y), c) -> (x,y)) $ pieces b)) [(x,y) | x <-[0.0..fromIntegral $ size b -1], y <-[0.0..fromIntegral $ size b -1]]


getAdjacentMoves ::[Position] -> [Position] -> [[Position]]
getAdjacentMoves all p = foldr (\(x,y) acc ->
                                    (foldr(\(d1, d2) acc1 ->
                                        let new_pos_1 = (x+d1,y+d2)
                                            new_pos_2 = (x+(d1+d1), y+(d2+d2)) in
                                              if new_pos_1 `elem` all ||  new_pos_2 `elem` all then new_pos_1:new_pos_2:acc1 else acc1
                                    )[] dirs
                           ):acc) [] p








--
--minimax :: Int -> GameTree -> Int
--minimax 0 (GameTree b col _)    = let x = evaluate b col in trace (show b ++ " " ++ show x) x
--minimax d (GameTree b col [])   = evaluate b col
--minimax d (GameTree b Black xs) = maximum $ map (minimax (d - 1)) (map snd (xs))
--minimax d (GameTree b White xs) = minimum $ map (minimax (d - 1)) (map snd (xs))

--
--minimax :: Int -> GameTree -> Int
--minimax 0 gt  = evaluate (game_board gt) (other (game_turn gt)) -- in trace (show (game_board gt) ++ " " ++ show x ++ " " ++show  (game_turn gt)) x
--minimax d gt | length (next_moves gt) == 0 = minBound :: Int
--             |  otherwise =  let score = minimum (map (minimax (d-1)) (map snd (next_moves gt))) in evaluate (game_board gt) (other (game_turn gt))
--
--


---  |  (game_turn gt) == Black =  maximum $ map (minimax (d - 1)) (map snd (next_moves gt)) -- let x = maximum $ map (minimax (d - 1)) (map snd (next_moves gt))  in trace (show (game_board gt) ++ " " ++ show x ++ " " ++show  (game_turn gt)) x ---
--             | (game_turn gt) == White =  minimum $ map (minimax (d - 1)) (map snd (next_moves gt)) -- let x = minimum $ map (minimax (d - 1)) (map snd (next_moves gt))  in trace (show (game_board gt) ++ " " ++ show x ++ " " ++show  (game_turn gt)) x

--


-- | fst (won (game_board gt)) == True =  10000--evaluate (game_board gt) (game_turn gt) in trace ("HERE" ++ show (game_board gt) ++ " "++ show x) x


--getNextPos:: Int -> Float -> Float
--getNextPos count dir | dir == 0 = 0
--                     | dir >  0 = dir - (fromIntegral $ count)
--                     | dir <  0 = dir + (fromIntegral $ count)
--
--checkWinPossible :: Int-> [Position] -> (Position, Direction) -> Bool
--checkWinPossible 2 _ _ = True
--checkWinPossible count all_p (p,d) | y `notElem` all_p = False
--                                   | otherwise = checkWinPossible (count+1) all_p (p,d)
--                                   where x = (fst p + (getNextPos count (fst d)), snd p + (getNextPos count (snd d)))
--                                         y = trace (show x) x

 -- if (fst p + (fst d1 )
--checkAdjacent :: Board ->[(Position)] -> Col -> Direction -> [Position]
--checkAdjacent b pos col dir = foldr (\(p1, p2) acc -> let new_pos = (p1+fst dir, p2+snd dir) in
--                                                     if  new_pos `elem` pos
--                                                        then new_pos:acc
--                                                     else acc
--                                ) [] pos































--generateMoves :: Board -> Col -> [(Position)]
--generateMoves b c = let x = valid in trace (show valid) [(2,2)]
--                      where all_pos = [(x,y) | x <- filter (`notElem` (map (\((x,y), c) -> (x,y)) $ pieces b)) [(x,y) | x <-[0.0..fromIntegral $ size b -1], y <-[0.0..fromIntegral $ size b -1]], y <- dirs]
--                            valid = filter (\((x,y),(d1,d2)) -> ((x+d1) >= 0 && (x+d1) <= s) && ((y+d2) >= 0 && (y+d2) <= s)) all_pos
--                            s = fromIntegral (size b)
--
--
--filterGoodMoves :: Board -> Col -> [(Position, Direction)] -> Bool
--filterGoodMoves b c [] = False
--filterGoodMoves b c (x:xs) = undefined


--checkPosition b p d c (target b) == True = True
--                           | otherwise = checkPositions b c (filter (`notElem` (map (\(x,y) -> ((x,y),d)) $ createLine b p d)) xs)
--                        where p = fst x
--                              d = snd x
--
--checkPosition :: Board -> Position -> Direction -> Col -> Int -> Bool
--checkPosition b p d c 1 = True
--checkPosition b p d c n | (fst p + fst d, snd p +snd d) `notElem` map (fst) (filter ((==c).snd) (pieces b)) = False
--                        | otherwise = checkPosition b (fst p + fst d, snd p +snd d) d c (n-1)
--
--



--
--foldr (\(p1, p2) acc -> \k -> if (p1, p2 )
--
--                            ) []  map fst $ filter ((==c).snd) (pieces b)
--
--                    where poss_moves =




--foldr (\x acc-> evalulateFullLine b (pieces b) col x) 0 dirs






--























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

