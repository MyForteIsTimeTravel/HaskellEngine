--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Collision (ballsToTheWall, countCollisions) where

import Graphics.Gloss
import Simulation
import Window
import Linear

-- | TO DO
-- ball to ball collisions
-- more

-- | detect and resolve ball to wall collisions
ballsToTheWall :: [BallState] -> [BallState]
ballsToTheWall []     = []
ballsToTheWall [x]    = [ballToTheWall x]
ballsToTheWall (x:xs) = [ballToTheWall x] ++ ballsToTheWall xs

-- | detect and resolve a single ball to wall collision
ballToTheWall :: BallState -> BallState
ballToTheWall ball = ball { 
    pos = (pos ball), 
    vel = vel', 
    acc = (acc ball), 
    rad = (rad ball), 
    col = col' }
        where 
            vel' = if (topBottomCollision (pos ball) (rad ball)) then ((x (vel ball)), (-0.8) * (y (vel ball)))
                   else if (leftRightCollision (pos ball) (rad ball)) then ((-0.8) * (x (vel ball)), (y (vel ball)))
                   else vel ball
                   
            col' = if (topBottomCollision (pos ball) (rad ball)) || 
                      (leftRightCollision (pos ball) (rad ball)) then col ball
                   else col ball  
                 
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

-- | tests if a single ball is against an edge
isBallToTheWall :: BallState -> Bool
isBallToTheWall ball = 
    (topBottomCollision (pos ball) (rad ball)) ||
    (leftRightCollision (pos ball) (rad ball))
        
-- | count the number of collisions present
countCollisions :: [BallState] -> [Bool] -- count this in renderer
countCollisions []     = []
countCollisions [x]    = []
countCollisions (x:xs) = [isBallToTheWall x] ++ countCollisions xs