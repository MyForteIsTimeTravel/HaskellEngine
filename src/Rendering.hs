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
               [(visualize (flowfield state))] ++ 
               (renderEntities (entities state)) ++ 
               (simulationDebug state)           ++ 
               (entityDebug (head (entities state))))

entityDebug :: EntityState -> [Picture]
entityDebug entity = [uiBackground, p, v, a, t, r]
    where
        uiBackground =
            Translate (-0.39 * fromIntegral width) (-0.195 * fromIntegral height) $ 
            Scale     (0.2 * fromIntegral width) (0.1 * fromIntegral height) $ 
            Color     (makeColor (0.32) (0.32) (0.32) (0.8)) $
            polygon   rectangleGeometry
        p = label
            ("position : : " ++ show (pos entity))
            ((-0.480 * fromIntegral width), (-0.260 * fromIntegral height))
        v = label
            ("velocity : : " ++ show (vel entity))
            ((-0.480 * fromIntegral width), (-0.230 * fromIntegral height))
        a = label
            ("acceleration : : " ++ show (acc entity))
            ((-0.480 * fromIntegral width), (-0.200 * fromIntegral height))
        t = label
            ("target : : " ++ show (tgt entity))
            ((-0.480 * fromIntegral width), (-0.170 * fromIntegral height))
        r = label
            ("rotation : : " ++ show (rot entity))
            ((-0.480 * fromIntegral width), (-0.140 * fromIntegral height))


simulationDebug :: GameState -> [Picture]
simulationDebug state = [uiBackground, status, entityCount, simulationTick, collisionCount]
    where 
        uiBackground = -- transparent black backboard
            Translate (-0.39 * fromIntegral width) (-0.39 * fromIntegral height) $ 
            Scale     (0.2 * fromIntegral width) (0.08 * fromIntegral height) $ 
            Color     (makeColor (0.32) (0.32) (0.32) (0.8)) $
            polygon   rectangleGeometry
            
        entityCount = label
            ("entities : : " ++ show (length (entities state)))
            ((-0.480 * fromIntegral width), (-0.440 * fromIntegral height))
            
        collisionCount = label
            ("Collisions : : " ++ show (countCollisions (entities state)))
            ((-0.480 * fromIntegral width), (-0.410 * fromIntegral height))
            
        simulationTick = label
            ("simulation tick : : " ++ show (tick state))
            ((-0.480 * fromIntegral width), (-0.380 * fromIntegral height))
            
        status = title
            (if (paused state) then "PAUSED" else ("RUNNING" ++ dotdotdot (tick state)))
            ((-0.480 * fromIntegral width), (-0.350 * fromIntegral height))
            
            
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
