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
module Collision (ballsToTheWall, ballsToBalls, countCollisions) where

import Graphics.Gloss
import Simulation
import Window
import Linear

-------------------------------------
-- Collision Detection
-------------------------------------

-- | detect and resolve ball to wall collisions
ballsToTheWall :: [BallState] -> [BallState]
ballsToTheWall []     = []
ballsToTheWall [x]    = [ballToTheWall x]
ballsToTheWall (x:xs) = [ballToTheWall x] ++ ballsToTheWall xs

-- | detect and resolve a single ball to wall collision
ballToTheWall :: BallState -> BallState
ballToTheWall ball = ball { pos = (pos ball), vel = vel', acc = (acc ball), rad = (rad ball), col = col' }
    where 
        vel' = if (topBottomCollision (pos ball) (rad ball)) then 
                   ((fst (vel ball)), (-0.8) * (snd (vel ball)))
               else if (leftRightCollision (pos ball) (rad ball)) then 
                   ((-0.8) * (fst (vel ball)), (snd (vel ball)))
               else
                   vel ball
                   
        col' = if (topBottomCollision (pos ball) (rad ball)) ||
                  (leftRightCollision (pos ball) (rad ball)) then
                  col ball
               else
                  col ball 
                  
isBallToTheWall :: BallState -> Bool
isBallToTheWall ball = 
                (topBottomCollision (pos ball) (rad ball)) ||
                (leftRightCollision (pos ball) (rad ball))
                            
                 
-- | detect and resolve vertical edge collision
topBottomCollision :: Linear.Vector2D -> Float -> Bool
topBottomCollision (_, y) radius = topCollision || bottomCollision where 
        topCollision    = y - radius <= -fromIntegral height / 2
        bottomCollision = y + radius >=  fromIntegral height / 2
        
-- | detect and resolve horizontal edge collision
leftRightCollision :: Linear.Vector2D -> Float -> Bool
leftRightCollision (x, _) radius = rightCollision || leftCollision where 
        leftCollision  = x - radius <= -fromIntegral width / 2
        rightCollision = x + radius >=  fromIntegral width / 2
        
-- | count the number of collisions present
countCollisions :: [BallState] -> [Bool] -- count this in renderer
countCollisions []     = []
countCollisions [x]    = []
countCollisions (x:xs) = [isBallToTheWall x] ++ countCollisions xs
        
-- | detect and resolve edge collisions
--edgeCollision :: BallState -> Bool
--edgeCollision state = 
        
---- \\ BROKEN // ----

-- | detect and resolve ball to ball collisions
ballsToBalls :: [BallState] -> [BallState]
ballsToBalls state = map ballToBall (pairBalls state)



-- | (phrasing) pair all possible state combinations
pairBalls :: [BallState] -> [(BallState, BallState)]
pairBalls l = [(x, y) | (x:ys) <- tails l, y <- ys]

tails :: [a] -> [[a]]
tails []     = []
tails [x]    = [[x]]
tails (x:xs) = [tail xs] ++ tails xs

ballToBall :: (BallState, BallState) -> BallState
ballToBall (a, b) = a {pos = (pos a), vel = (avx', avy'), acc = (acc a), rad = (rad a), col = col a}
        where -- old velocities
              (avx, avy) = vel a
              (bvx, bvy) = vel b
              -- new velocities (not 100% correct)
              avx' = if (distance (pos a) (pos b) * 0.5) > rad a then avx else bvx - avx
              avy' = if (distance (pos a) (pos b) * 0.5) > rad a then avy else bvy - avy
              
---- // BROKEN \\ ----