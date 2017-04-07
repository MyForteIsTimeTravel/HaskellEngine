--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Rendering (render) where

-----------------------------------------
-- Renders Simulation State
-----------------------------------------

import Graphics.Gloss
import FlowField
import Simulation
import Collision
import Geometry
import Window
import Linear
import Entity
import UI

-----------------------------------------
-- | render UI and actors
-----------------------------------------
render :: GameState -> Picture
render state = pictures (
              -- [(visualize (flowfield state))]   ++ 
               (renderEntities (entities state)) ++ 
               (renderInterface (state)))

-----------------------------------------
-- | renders the interface for the state
-----------------------------------------
renderInterface :: GameState -> [Picture]
renderInterface state = traverseInterface (interface state)

-----------------------------------------
-- | traverses a list of UINodes and renders
-- | each.
-----------------------------------------
traverseInterface :: [UINode] -> [Picture]
traverseInterface []     = []
traverseInterface [x]    = (renderNode x)
traverseInterface (x:xs) = (renderNode x) ++ (traverseInterface xs)

-----------------------------------------
-- | renders a single node
-----------------------------------------
renderNode :: UINode -> [Picture]
renderNode n = [background, pictures (elements)]
    where background =
               Translate (x (uipos n)) (y (uipos n)) $ 
               Scale     (w (size n)) (h (size n)) $ 
               Color     (makeColor (0.32) (0.32) (0.32) (0.8)) $
               polygon   rectangleGeometry
           elements = (renderElements (elm n))

-----------------------------------------
-- | renders a list of elements
-----------------------------------------
renderElements :: [UIElement] -> [Picture]
renderElements []     = []
renderElements [x]    = [renderTitle x]
renderElements (x:xs) = [renderElement x] ++ renderElements xs

-----------------------------------------
-- | renders a non-title element
-----------------------------------------
renderElement :: UIElement -> Picture
renderElement e = label (txt e) (loc e)
 
 -----------------------------------------
-- | renders a title element
-----------------------------------------
renderTitle :: UIElement -> Picture
renderTitle t = title (txt t) (loc t)

-----------------------------------------
-- | render actors, plural
-----------------------------------------
renderEntities :: [EntityState] -> [Picture]
renderEntities state = concat (map renderArrow state)
-----------------------------------------
-- | render actor as ball
-----------------------------------------
renderBall :: EntityState -> [Picture]
renderBall ball = [fill, outline, radius]
    where   
        fill =
            Translate (x (pos ball)) (y (pos ball)) $
            Color     (makeColor (0.32) (0.32) (0.32) (0.8))  $
            circleSolid (rad ball)
        outline =
            Translate (x (pos ball)) (y (pos ball)) $
            Color     black  $
            thickCircle (rad ball) (1)
        radius = 
            Translate (x (pos ball)) (y (pos ball)) $
            Color     (white) $
            line      [(0, 0), ((rad ball), 0) `Linear.rotate` (rot ball)]
-----------------------------------------
-- | render actor as box
-----------------------------------------
renderBox :: EntityState -> [Picture]
renderBox box = [sprite, direction]
    where
        sprite =
            Translate (x (pos box)) (y (pos box)) $
            Color     (makeColor (0.32) (0.32) (0.32) (0.8))  $
            Rotate    (-toDegrees (heading (vel box))) $
            Scale     (8) (4) $
            polygon   rectangleGeometry
        direction =
            Translate (x (pos box)) (y (pos box)) $
            Color     (white) $
            line      [(0, 0), (4 * magnitude (vel box), 0) `Linear.rotate` (rot box)]
-----------------------------------------
-- | render actor as a tri
-----------------------------------------
renderTri :: EntityState -> [Picture]
renderTri t = [sprite]
    where
        sprite =
            Translate (x (pos t)) (y (pos t)) $
            Color     (makeColor (0.32) (0.32) (0.32) (0.8))  $
            Rotate    (-(toDegrees (heading (vel t)) - 90)) $
            Scale     (10) (20) $
            polygon   triangleGeometry
-----------------------------------------
-- | render actor as an arrow of its velocity
-----------------------------------------
renderArrow :: EntityState -> [Picture]
renderArrow t = [sprite]
    where
        sprite =
            Translate (x (pos t)) (y (pos t)) $
            Color     (makeColor (0.32) (0.32) (0.32) (0.8))  $
            Rotate    (-(toDegrees (heading (vel t)))) $
            Scale     (20) (20) $
            polygon   arrowGeometry
