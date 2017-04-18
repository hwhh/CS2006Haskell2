module Draw(drawWorld) where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Game
import Graphics.Gloss.Juicy

import Board


fieldSize@(width, height) = (720, 720) :: (Float, Float)

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.

--65, 65

drawWorld :: [Picture] -> World -> IO Picture
drawWorld p w|  fst (won(board w))   = return $ translate (-330.0) 0.0 (text ((show $ fromJust $ snd game_won)++" Wins" )) --
             | otherwise = return $ pictures((p!!0):drawGrid(b):drawPices b p :[])
                   where b = board w
                         game_won = won b


drawGrid :: Board -> Picture --110, 80
drawGrid b =pictures[uncurry translate (cellToScreen b x y (24, 24)) $ color white $ rectangleWire (width/(fromIntegral s))  (height/(fromIntegral s)) | x <- [0 .. s-1], y <- [0 ..s-1]]
                    where s = (size b)

drawPNG ::  Col -> Picture
drawPNG col | col == Black = png "Pieces/b1o.png"
            | col == White = png "Pieces/w1o.png"


drawPices::  Board -> [Picture] -> Picture
drawPices b p = pictures(foldr (\((f, s), c) acc ->
                        if c == Black then
                            (uncurry translate (cellToScreen b f s (0,0)) (p!!2)) : acc
                        else
                            (uncurry translate (cellToScreen b f s (0,0)) (p!!1)) : acc
                        )
                [] $ pieces b)

cellToScreen :: Board -> Int -> Int -> (Float, Float)->(Float, Float)
cellToScreen b x y (x_offset, y_offset)= (x_start + ((fromIntegral x) * (width / fromIntegral (size b)) + x_offset), y_start - ((fromIntegral y) * (height / fromIntegral (size b))) -y_offset)
                    where x_start = (-(width/2))
                          y_start = (height/2)


