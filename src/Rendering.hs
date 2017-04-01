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
render :: [BallState] -> Picture
render state = pictures ((renderBalls state) ++ [uiBackground, ballCount, simulationTick, collisionCount])
    where uiBackground =
              Translate (-0.39 * fromIntegral width) (-0.39 * fromIntegral height) $ 
              Scale     (0.2 * fromIntegral width) (0.08 * fromIntegral height) $ 
              Color     (makeColor (0.32) (0.32) (0.32) (0.8)) $
              polygon   rect
    
          ballCount = 
              Translate (-0.480 * fromIntegral width) (-0.440 * fromIntegral height) $ 
              Scale     (0.12) (0.12) $ 
              Color     (dark white) $
              Text      ("balls - " ++ show (length state))
            
          collisionCount = 
              Translate (-0.480 * fromIntegral width) (-0.410 * fromIntegral height) $ 
              Scale     (0.12) (0.12) $ 
              Color     (dark white) $
              Text      ("Collisions - " ++ show (countCollisions state))
            
          simulationTick = 
              Translate (-0.480 * fromIntegral width) (-0.380 * fromIntegral height) $ 
              Scale     (0.12) (0.12) $ 
              Color     (dark white) $
              Text      ("simulation tick - TO DO")
            
-----------------------------------------
-- | render actors, plural
-----------------------------------------
renderBalls :: [BallState] -> [Picture]
renderBalls state = concat (map renderBall state)

-----------------------------------------
-- | render actor, singular
-----------------------------------------
renderBall :: BallState -> [Picture]
renderBall ball = [fill, outline]
    where   
        fill =
            Translate (x (pos ball)) (y (pos ball)) $
            Color     (makeColor (0.32) (0.32) (0.32) (0.8))  $
            circleSolid (rad ball)
    
        outline =
            Translate (x (pos ball)) (y (pos ball)) $
            Color     (col ball)  $
            thickCircle (rad ball) (3)