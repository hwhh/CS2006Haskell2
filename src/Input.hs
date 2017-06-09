module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import SaveLoad
import Data.Binary
import Data.ByteString.Lazy as B
import Draw


import Debug.Trace

fieldSize@(width, height) = (720, 720) :: (Float, Float)


--if (pieces b) == []
--then return $ w {board = new_board, turn = other col}
--else return $ w {board = new_board{score = updateScore b col }, turn = other col}

handleInput :: Event -> World -> IO World
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w =  case makeMove b col (f, s) of
                                                                          Just new_board -> case fst $ won new_board of
                                                                                 True -> return $ w {board = new_board, turn = other col}
                                                                                 False ->return $ w {board = new_board, turn = other col}
                                                                          Nothing -> return w

                                                                 where b = board w
                                                                       col = turn w
                                                                       (f,s) = screenToCell b x y
handleInput (EventKey (Char k) Down _ _) w
    = case k of
           'n' -> return $ w{board = initBoard(flags w) , turn=Black}
           'u' -> undo w
           's' -> do (trace("Saving game") B.writeFile "SaveFile.dat" (encode w)); return w
           'l' -> do file <- B.readFile "SaveFile.dat"; return $ (trace("Loading game") decode file)
           'h' -> do trace ("hints") return $ w{hints = True}
           _ -> return w

handleInput (EventKey (Char k) Up _ _) w
    = case k of
        'h' -> do trace ("no hints") return $ w{hints = False}
        _ -> return w


handleInput e w = return w

-- | Converts a screen click to a cell
screenToCell :: Board -> Float -> Float -> Position
screenToCell b x y =(round((x / (width/ (s*2)) + s) / 2),  round (((-y)/ (height/ (s*2)) + s) /2))
            where s = fromIntegral $ size b


{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}

