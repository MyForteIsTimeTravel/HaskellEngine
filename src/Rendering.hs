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
import Window

-----------------------------------------
-- | render UI and actors
-----------------------------------------
render :: [BallState] -> Picture
render state = pictures ((renderBalls state) ++ [ballCount, collisionCount])
    where ballCount = 
              Translate (-0.485 * fromIntegral width) (-0.475 * fromIntegral height) $ 
              Scale     (0.15) (0.15) $ 
              Color     (light black) $
              Text      ("balls: " ++ show (length state))
            
          collisionCount = 
              Translate (-0.485 * fromIntegral width) (-0.475 * fromIntegral height + 32) $ 
              Scale     (0.15) (0.15) $ 
              Color     (light black) $
              Text      ("Collisions: " ++ show (countCollisions state))

-----------------------------------------
-- | render actors, plural
-----------------------------------------
renderBalls :: [BallState] -> [Picture]
renderBalls state = map renderBall state

-----------------------------------------
-- | render actor, singular
-----------------------------------------
renderBall :: BallState -> Picture
renderBall ball = uncurry 
    translate   (pos ball) $ 
    color       (col ball) $ 
    thickCircle (rad ball) (2)