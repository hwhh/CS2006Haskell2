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

handleInput (EventMotion (x, y)) b  = trace ("Mouse moved to: " ++ show (x,y)) b

handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w = case makeMove b col (f, s) of
                                                                      Just new_board ->  trace ("1.  Left button pressed at: " ++ (show $ (f, s))) w {board = new_board, turn = other col}
                                                                      Nothing -> trace ("2. Left button pressed at: " ++ (show $ (f, s))) w
                                                                where b = board w
                                                                      col = turn w
                                                                      (f, s) = screenToCell b x y


--other $ turn w w {pieces = ((, other $ turn w):pieces b)}, other $ turn w}-- --  b


handleInput (EventKey (Char k) Down _ _) b
    = trace ("Key " ++ show k ++ " down") b
handleInput (EventKey (Char k) Up _ _) b
    = trace ("Key " ++ show k ++ " up") b
handleInput e b = b


--320, 240 == 0, 0
screenToCell :: Board -> Float -> Float -> (Float, Float)
screenToCell b x y = (fromIntegral $ ceiling $ x / (width/ s) + offset ,  fromIntegral $ ceiling $ (-y)/ (height/ s) + offset)
            where s = fromIntegral $ size b
                  offset = s / 3


createWorld :: World -> Board -> Col -> World
createWorld w b c = w {board = b, turn = c}

{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}

