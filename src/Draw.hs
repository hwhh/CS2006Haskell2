module Draw(drawWorld) where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Game
import Board


fieldSize@(width, height) = (660, 480) :: (Float, Float)

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.


drawWorld :: World -> Picture
drawWorld w |  False    = translate (-330.0) 0.0 (text ((show $ fromJust $ snd game_won)++" Wins" )) --fst game_won
            | otherwise = pictures(drawGrid(b):drawPices(b):[])
                   where b = board w
                         game_won = won b

drawGrid :: Board -> Picture
drawGrid b = pictures[uncurry translate (cellToScreen b x y (110 , 80)) $ color black $ rectangleWire (width/(fromIntegral s))  (height/(fromIntegral s)) | x <- [0 .. s-1], y <- [0 ..s-1]]
                   where s = (size b)

drawPNG ::  Col -> Picture
drawPNG col
            | col == Black = png "Pieces/b1o.png"
            | col == White = png "Pieces/w1o.png"

drawPices::  Board -> Picture
drawPices b = pictures(foldr (\((f, s), c) acc ->
                        if c == Black then
                            (uncurry translate (cellToScreen b f s (0,0)) (drawPNG Black )) : acc
                        else
                            (uncurry translate (cellToScreen b f s (0,0)) (drawPNG White )) : acc
                        )
                [] $ pieces b)

cellToScreen :: Board -> Int -> Int -> (Float, Float)->(Float, Float)
cellToScreen b x y (x_offset, y_offset)= (x_start + ((fromIntegral x) * (width / fromIntegral (size b)) + x_offset), y_start - ((fromIntegral y) * (height / fromIntegral (size b))) -y_offset)
                    where x_start = (-(width/2))
                          y_start = (height/2)


