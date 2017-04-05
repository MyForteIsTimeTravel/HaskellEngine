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
render state = pictures ((renderEntities (entities state)) ++ [uiBackground, status, entityCount, simulationTick, collisionCount])
    where 
        uiBackground = -- transparent black backboard
            Translate (-0.39 * fromIntegral width) (-0.39 * fromIntegral height) $ 
            Scale     (0.2 * fromIntegral width) (0.08 * fromIntegral height) $ 
            Color     (makeColor (0.32) (0.32) (0.32) (0.8)) $
            polygon   rect
            
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
renderEntities state = concat (map renderBox state)
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
            polygon   rect
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
            polygon   tri
            
-----------------------------------------
-- | render actor as an arrow of its velocity
-----------------------------------------
renderArrow :: EntityState -> [Picture]
renderArrow t = [tip, body]
    where
        tip =
            Translate (x (pos t)) (y (pos t)) $
            Color     (makeColor (0.32) (0.32) (0.32) (0.8))  $
            Rotate    (-(toDegrees (heading (vel t)) - 90)) $
            Scale     (8) (16) $
            polygon   tri
        body = 
            Translate (x (pos t)) (y (pos t)) $
            Color     (makeColor (0.32) (0.32) (0.32) (0.8))  $
            Rotate    (-toDegrees (heading (vel t))) $
            Scale     (16) (2) $
            Translate (-1) (0) $
            polygon   rect