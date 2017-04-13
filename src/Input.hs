module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI

import Debug.Trace

fieldSize@(width, height) = (660, 480) :: (Float, Float)

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInput :: Event -> World -> World

handleInput (EventMotion (x, y)) b  =  b

handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w | turn w == White = case makeMove b col (f, s) of
                                                                                      Just new_board -> case fst $ won new_board of
                                                                                             True -> trace ("Game won " ++ (show $ (f, s))) w {board = new_board, turn = other col}
                                                                                             False ->trace ("Game not won " ++ (show $ (f, s))) w {board = new_board, turn = other col}
                                                                                      Nothing -> trace ("2. Left button pressed at: " ++ (show $ (f, s))) w
                                                                                where b = board w
                                                                                      col = turn w
                                                                                      (f, s) = screenToCell b x y
handleInput (EventKey (Char k) Down _ _) b
    = trace ("Key " ++ show k ++ " down") b
handleInput (EventKey (Char k) Up _ _) b
    = trace ("Key " ++ show k ++ " up") b
handleInput e b = b


screenToCell :: Board -> Float -> Float -> (Float, Float)
screenToCell b x y = (fromIntegral $ ceiling $ x / (width/ s) + offset ,  fromIntegral $ ceiling $ (-y)/ (height/ s) + offset)
            where s = fromIntegral $ size b
                  offset = s / 3


--resolveCoordinates :: Float -> Float -> Float
--resolveCoordinates c i = if abs(cl - c) > abs(fl - c)
--                            then trace ("returned floor " ++(show $ fl)) fl
--                         else
--                            trace ("returned ceiling " ++(show $ cl)) cl
--                       where
--                        cl =  i * fromIntegral(ceiling(c / i))
--                        fl =  i * fromIntegral(floor(c / i))

{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}

