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
import Window
import Linear

-- | TO DO
-- ball to ball collisions
-- more

-------------------------------------
-- Detect and Resolve
-------------------------------------

-----------------------------------------------------------
-- | detect and resolve all collisions in the given state
-- |
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
ballToTheWall ball = ball { 
    pos = (pos ball), 
    vel = vel', 
    acc = (acc ball), 
    rad = (rad ball), 
    col = (col ball) }
        where vel' = if topCollision    (pos ball) (rad ball) then ((x (vel ball)), (-0.8) * (y (vel ball))) else 
                     if bottomCollision (pos ball) (rad ball) then ((x (vel ball)), (-0.8) * (y (vel ball))) else 
                     if leftCollision   (pos ball) (rad ball) then ((-0.8) * (x (vel ball)), (y (vel ball))) else
                     if rightCollision  (pos ball) (rad ball) then ((-0.8) * (x (vel ball)), (y (vel ball))) else vel ball

-- | 
topCollision    :: Vector2D -> Float -> Bool; topCollision    pos rad = (y pos) - rad <= -fromIntegral height / 2
bottomCollision :: Vector2D -> Float -> Bool; bottomCollision pos rad = (y pos) + rad >=  fromIntegral height / 2
leftCollision   :: Vector2D -> Float -> Bool; leftCollision   pos rad = (x pos) - rad <= -fromIntegral width  / 2
rightCollision  :: Vector2D -> Float -> Bool; rightCollision  pos rad = (x pos) + rad >=  fromIntegral width  / 2

------------------------------
-- | Object Collisions
------------------------------
checkObjects :: [BallState] -> [BallState]
checkObjects []     = []
checkObjects [x]    = [x]
checkObjects (x:xs) = [ballsToBalls x xs] ++ checkObjects xs

ballsToBalls :: BallState -> [BallState] -> BallState
ballsToBalls ball scene = ball -- TO-DO


-------------------------------------
-- Metrics
-------------------------------------

-- | tests if a single ball is against an edge
isBallToTheWall :: BallState -> Bool
isBallToTheWall ball = 
    (topCollision    (pos ball) (rad ball)) || 
    (bottomCollision (pos ball) (rad ball)) ||
    (leftCollision   (pos ball) (rad ball)) || 
    (rightCollision  (pos ball) (rad ball))
        
-- | count the number of collisions present
countCollisions :: [BallState] -> Int
countCollisions state = (length (filter (==True) (collect state)))

collect :: [BallState] -> [Bool] -- count this in renderer
collect []     = []
collect [x]    = []
collect (x:xs) = [isBallToTheWall x] ++ collect xs