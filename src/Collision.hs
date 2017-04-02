--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Collision (checkCollisions, countCollisions) where

import Graphics.Gloss
import Simulation
import Physics
import Window
import Linear
import Ball

-------------------------------------
-- Detect and Resolve
-------------------------------------
checkCollisions :: [BallState] -> [BallState]
checkCollisions scene = checkObjects (checkEdges scene)

------------------------------
-- | Edge Collisions
------------------------------
checkEdges :: [BallState] -> [BallState]
checkEdges []     = []
checkEdges [x]    = [ballToTheWall x]
checkEdges (x:xs) = [ballToTheWall x] ++ checkEdges xs

-- | detect and resolve a single ball to wall collision
ballToTheWall :: BallState -> BallState
ballToTheWall ball = ball { vel = vel' }
        where vel' = 
                -- hit floor
                if (y (pos ball)) - (rad ball) <= -fromIntegral height / 2 then 
                    ((x (vel ball)), (-0.8) * (y (vel ball))) else 
                -- hit roof
                if (y (pos ball)) + (rad ball) >=  fromIntegral height / 2 then 
                    ((x (vel ball)), -10) else 
                -- hit left wall
                if (x (pos ball)) - (rad ball) <= -fromIntegral width  / 2 then 
                    ((-0.8) * (x (vel ball)), (y (vel ball))) else
                -- hit right wall
                if (x (pos ball)) + (rad ball) >=  fromIntegral width  / 2 then 
                    ((-0.8) * (x (vel ball)), (y (vel ball))) else vel ball

------------------------------
-- | Object Collisions
------------------------------
checkObjects :: [BallState] -> [BallState]
checkObjects []     = []
checkObjects [x]    = [x]
checkObjects (x:xs) = [ballToBalls x xs] ++ checkObjects xs

ballToBalls :: BallState -> [BallState] -> BallState
ballToBalls ball scene = ball { acc = foldl (add) (acc ball) (map (ballToBall ball) (scene)) }
                            
-- | check if the balls are intersecting and push one away
ballToBall :: BallState -> BallState -> Vector2D
ballToBall a b = 
    if (distance (pos a) (pos b)) < ((rad a) + (rad b)) then 
        (normalize ((pos a) `sub` (pos b))) `Linear.scale` ((distance (pos a) (pos b)) - ((rad a)))
    else (0, 0)

-------------------------------------
-- Metrics
-------------------------------------
-- | tests if a single ball is against an edge
isBallToTheWall :: BallState -> Bool
isBallToTheWall ball = 
    ((y (pos ball)) - (rad ball) <= -fromIntegral height / 2) || 
    ((y (pos ball)) + (rad ball) >=  fromIntegral height / 2) ||
    ((x (pos ball)) - (rad ball) <= -fromIntegral width  / 2) || 
    ((x (pos ball)) + (rad ball) >=  fromIntegral width  / 2)
-- | count the number of collisions present
countCollisions :: [BallState] -> Int
countCollisions state = (length (filter (==True) (collect state)))
-- | collect collisions im scene
collect :: [BallState] -> [Bool]
collect []     = []
collect [x]    = []
collect (x:xs) = [isBallToTheWall x] ++ collect xs