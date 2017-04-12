module Draw(drawWorld) where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Game
import Board


fieldSize@(width, height) = (420, 420) :: (Float, Float)

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
drawGrid b = pictures[uncurry translate (cellToScreen b x y o) $ color black $ rectangleWire (width/s)  (height/s) | x <- [0 .. s-1], y <- [0 ..s-1]]
                   where s = fromIntegral (size b)
                         o = (35, 35)

drawPNG ::  Col -> Picture
drawPNG col
            | col == Black = png "Pieces/b1o.png"
            | col == White = png "Pieces/w1o.png"

drawPices::  Board -> Picture
drawPices b = pictures(foldr (\((f, s), c) acc ->
                        if c == Black then
                            (uncurry translate (cellToScreen b f s o) (drawPNG Black )) : acc
                        else
                            (uncurry translate (cellToScreen b f s o) (drawPNG White )) : acc
                        )
                [] $ pieces b) where o = (70,70)

cellToScreen :: Board -> Float -> Float -> (Float, Float) -> (Float, Float)
cellToScreen b x y o = (x_start + (x * (width / fromIntegral (size b)) + fst o), y_start - (y * (height / fromIntegral (size b))) -snd o)
                    where x_start = (-(width/2))
                          y_start = (height/2)
--                          x_offset = 70
--                          y_offset = 70



--drawPieces :: Board -> Picture
--drawPieces b = pictures (drawBlackPieces b: drawWhitePieces b :[])


--drawBlackPieces :: Board -> Picture
--drawBlackPieces b = pictures [uncurry translate(cellToScreen b (fromIntegral (fst (x))) (fromIntegral(snd (x))))$ color black $ circle 10
--                                            | x <- (foldr (\((f, s), c) acc -> (f, s):acc) [] $ filter ((== Black).snd) $ pieces b)]
--
--drawWhitePieces :: Board -> Picture
--drawWhitePieces b = pictures [uncurry translate(cellToScreen b (fromIntegral (fst (x))) (fromIntegral(snd (x))))$ color red $ circle 10
--                                            | x <- (foldr (\((f, s), c) acc -> (f, s):acc) [] $ filter ((== White).snd) $ pieces b)]

