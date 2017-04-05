--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module FlowField (flowField) where

import Graphics.Gloss

import Linear
import Window

flowField :: Int -> [[Vector2D]]
flowField res = [flowRow res]
-- col = width / res
-- row = height / res

flowRow :: Int -> [Vector2D]
flowRow width = [(1, 0)]

{--

visualize :: [[Vector2D]] -> Picture
visualize field = 

arrow :: Vector2D -> Vector2D -> Picture
arrow dir pos = [body, tip]
    where
        body =
            Translate (x (pos)) (y (pos)) $
            Color     (black) $
            line      [(0, 0), (4 * (magnitude dir), 0) `Linear.rotate` (heading dir)]
        tip = 
            Translate (x (pos)) (y (pos)) $
            Color     (black) $
            tri       [(0, 0), (4 * (magnitude dir), 0) `Linear.rotate` (heading dir)]
            
--}