--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Geometry (rectangleGeometry, triangleGeometry, arrowGeometry, thinArrowGeometry) where

import Linear

----------------------------------
-- | vertices of a rectangle
----------------------------------
rectangleGeometry :: [Vector2D]
rectangleGeometry = [(-1, -1), (1, -1), (1,  1), (-1,  1)]

----------------------------------
-- | vertices of a triangle
----------------------------------
triangleGeometry :: [Vector2D]
triangleGeometry = [(-1, -0.5), (0, 0.5), (1, -0.5)]

----------------------------------
-- | vertices of an arrow
----------------------------------
arrowGeometry :: [Vector2D]
arrowGeometry = 
    [(0.2, 0.1),  (0.2, 0.25), (0.4, 0), (0.2, -0.25), 
     (0.2, -0.1), (-0.4, -0.065), (-0.4, 0.065), (0.2, 0.1)]
     
     
----------------------------------
-- | vertices of a line arrow
----------------------------------
-- nope
thinArrowGeometry :: [Vector2D]
thinArrowGeometry = 
    [(-0.5, 0),  (0.5, 0),  --   -
     (0.5,  0),  (0.25, -0.5), (0.25, -0.5), (0.5, 0),
    
     (0.5,  0),  (0.25, 0.5), (0.25, 0.5), (0.5, 0)]