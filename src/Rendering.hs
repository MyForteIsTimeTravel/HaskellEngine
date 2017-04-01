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

-----------------------------------------
-- | render UI and actors
-----------------------------------------
render :: GameState -> Picture
render state = pictures ((renderBalls (actors state)) ++ [uiBackground, pausedLabel, ballCount, simulationTick, collisionCount])
    where uiBackground =
              Translate (-0.39 * fromIntegral width) (-0.39 * fromIntegral height) $ 
              Scale     (0.2 * fromIntegral width) (0.08 * fromIntegral height) $ 
              Color     (makeColor (0.32) (0.32) (0.32) (0.8)) $
              polygon   rect

          ballCount = 
              Translate (-0.480 * fromIntegral width) (-0.440 * fromIntegral height) $ 
              Scale     (0.12) (0.12) $ 
              Color     (dark white) $
              Text      ("balls - " ++ show (length (actors state)))
            
          collisionCount = 
              Translate (-0.480 * fromIntegral width) (-0.410 * fromIntegral height) $ 
              Scale     (0.12) (0.12) $ 
              Color     (dark white) $
              Text      ("Collisions - " ++ show (countCollisions (actors state)))
            
          simulationTick = 
              Translate (-0.480 * fromIntegral width) (-0.380 * fromIntegral height) $ 
              Scale     (0.12) (0.12) $ 
              Color     (dark white) $
              Text      ("simulation tick - " ++ show (tick state))
              
          pausedLabel =
              Translate (-0.480 * fromIntegral width) (-0.350 * fromIntegral height) $ 
              Scale     (0.12) (0.12) $ 
              Color     (black) $
              Text      (if (paused state) then "PAUSED" else "RUNNING")
            
-----------------------------------------
-- | render actors, plural
-----------------------------------------
renderBalls :: [BallState] -> [Picture]
renderBalls state = concat (map renderBall state)

-----------------------------------------
-- | render actor, singular
-----------------------------------------
renderBall :: BallState -> [Picture]
renderBall ball = [fill, outline, radius]
    where   
        fill =
            Translate (x (pos ball)) (y (pos ball)) $
            Color     (makeColor (0.32) (0.32) (0.32) (0.8))  $
            circleSolid (rad ball)
    
        outline =
            Translate (x (pos ball)) (y (pos ball)) $
            Color     (col ball)  $
            thickCircle (rad ball) (3)
            
        radius = 
            Translate (x (pos ball)) (y (pos ball)) $
            Color     (black) $
            line      [(0, 0)] -- TO DO