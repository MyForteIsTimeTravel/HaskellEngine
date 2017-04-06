--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module FlowField (makeField, FlowPoint, visualize, findFlow) where

import Graphics.Gloss

import Data.List
import Data.Ord

import Geometry
import Window
import Linear

resX :: Int
resX = 30

resY :: Int
resY = 30

-------------------------------------
-- | a point in the flow field
-------------------------------------
data FlowPoint = Point {
    pos :: Vector2D, -- position of the flow point in the world
    dir :: Vector2D  -- direction of the flow point
}

-----------------------------------------
-- | returns a flow field with the given
-- | width and height mapped to the screen
-- | size (eventually)
-----------------------------------------
makeField :: Int -> Int -> [FlowPoint]
makeField w 0 = []
makeField w h = (addRow w h) ++ (makeField (w) (h - 1) )

-----------------------------------------
-- | returns a row of flow points
-----------------------------------------
addRow :: Int -> Int -> [FlowPoint]
addRow 0 h = []
addRow w h = [(addPoint w h)] ++ (addRow (w - 1) (h) )

-----------------------------------------
-- | returns a single flow point
-----------------------------------------
addPoint :: Int -> Int -> FlowPoint
addPoint w h = Point { pos = pos',  dir = dir' }
    where
        pos' = (bottomLeftX + (fromIntegral ((width `div` resX) * w)), bottomY + (fromIntegral ((height `div` resY) * h)))
        --dir' = (0, 0) `sub` pos'
        dir' = (0, 1)
-----------------------------------------
-- | return the flow at the given location
-----------------------------------------
findFlow :: Vector2D -> [FlowPoint] -> Vector2D
findFlow loc field = dir (head (sortBy (flowSort loc) (field)))

flowSort :: Vector2D -> FlowPoint -> FlowPoint -> Ordering
flowSort loc lhs rhs = compare (distance (pos lhs) loc) (distance (pos rhs) loc)

-----------------------------------------
-- | display the flow field graphically
-----------------------------------------
visualize :: [FlowPoint] -> Picture
visualize field = pictures (map arrow field)

arrow :: FlowPoint -> Picture
arrow point =  
            Translate (x (pos point)) (y (pos point)) $
            Color     (makeColor (0.32) (0.32) (0.32) (0.2))  $
            Rotate    (-toDegrees (heading (dir point))) $
            Scale     (40) (40) $
            polygon   arrowGeometry