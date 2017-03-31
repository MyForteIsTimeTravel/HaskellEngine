--------------------------------------------------------------
--------------------------------------------------------------
--
--  http://andrew.gibiansky.com/blog/haskell/haskell-gloss/
--
--  Graphics and Simulation in haskell
--  Ryan Needham
--
--------------------------------------------------------------
--------------------------------------------------------------
module Rendering (render) where

import Graphics.Gloss
import Simulation
import Collision
import Window

-------------------------------------
-- Render Simulation
-------------------------------------

-- | render UI and actors
render :: [BallState] -> Picture
render state = pictures ((renderBalls state) ++ [ballCount, collisionCount])
    where
        ballCount = 
            Translate (-0.485 * fromIntegral width) (-0.475 * fromIntegral height) $ 
            Scale     (0.15) (0.15) $ 
            Text      ("balls: " ++ show (length state))
          
          
        collisionCount = 
            Translate (-0.485 * fromIntegral width) (-0.475 * fromIntegral height + 32) $ 
            Scale     (0.15) (0.15) $ 
            Text      ("Collisions: " ++ show (length (filter (==True) (countCollisions state))))

-- | render actors, plural
renderBalls :: [BallState] -> [Picture]
renderBalls state = map renderBall state

-- | render actor, singular
renderBall :: BallState -> Picture
renderBall ball = uncurry translate (pos ball) $ color (col ball) $ circleSolid (rad ball)