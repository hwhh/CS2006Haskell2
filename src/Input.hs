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

handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w = if (turn w == h_player w) then
                                                                     case makeMove b col (f, s) of --
                                                                          Just new_board -> case fst $ won new_board of
                                                                                 True -> trace ("Game won") w {board = new_board, turn = other col}
                                                                                 False ->w {board = new_board, turn = other col}
                                                                          Nothing -> trace ("f,s: " ++ show (f,s) ++"x,y: " ++ show (x, y)) w -- show hint right click ?
                                                                 else w
                                                                 where b = board w
                                                                       col = turn w
                                                                       (f,s) = screenToCell b x y
handleInput (EventKey (Char k) Down _ _) b
    = trace ("Key " ++ show k ++ " down") b



handleInput (EventKey (Char k) Up _ _) b
    = trace ("Key " ++ show k ++ " up") b
handleInput e b = b


screenToCell :: Board -> Float -> Float -> Position
screenToCell b x y =(round((x / (width/ (s*2)) + s) / 2),  round (((-y)/ (height/ (s*2)) + s) /2))
            where s = fromIntegral $ size b


{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}

