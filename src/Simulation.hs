--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Simulation (BallState, pos, vel, acc, rad, col, count, initialState, advance, applyForces) where

-------------------------------------
-- Simulate World
-------------------------------------

import Graphics.Gloss
import Linear

-------------------------------------
-- | the data model for a ball
-------------------------------------
data BallState = Ball {
    pos :: Linear.Vector2D,  -- the ball's position in 2D space
    vel :: Linear.Vector2D,  -- the ball's velocity vector
    acc :: Linear.Vector2D,  -- the ball's acceleration
    rad :: Float,            -- the ball's radius, also functions as mass
    col :: Color             -- the ball's colour
} deriving Show

-------------------------------------
-- | the number of balls in the 
-- | simualtion
-------------------------------------
count :: Int; count = 32

-------------------------------------
-- | the highest attainable speed in 
-- | the simulation
-------------------------------------
maxSpeed :: Float; maxSpeed = 10.0

-------------------------------------
-- | return a list of n random ball
-- | states
-------------------------------------
initialState :: Int -> [BallState]
initialState 0 = []
initialState 1 = addBall []
initialState n = addBall (initialState (n - 1))

-------------------------------------
-- | add a ball to the state's list 
-- | of balls
-------------------------------------
addBall :: [BallState] -> [BallState]
addBall state = state ++ [Ball { 
    pos = (0,  0), 
    vel = if (mod (length state) 2) == 0 then 
              (fromIntegral (length state), 
              (fromIntegral (length state) * fromIntegral (length state)) * 0.32) 
          else 
              (-(fromIntegral (length state)), 
              ( fromIntegral (length state) * (fromIntegral (length state))) * 0.32),
    --vel = (0.0, 0.0),
    acc = (-0.001, 0.01), 
    rad = 4.0 + (fromIntegral (length state)) * 0.01,
    col = makeColor ((fromIntegral ((length state)) * 0.01)) 
                    ((fromIntegral ((length state)) * 0.01)) 
                    ((fromIntegral ((length state)) * 0.01)) (0.4)
}]

-------------------------------------
-- | advance the whole simulation the 
-- | given number of seconds
-------------------------------------
advance :: Float -> [BallState] -> [BallState]
advance seconds []     = []
advance seconds [x]    = [updateBall seconds x]
advance seconds (x:xs) = [updateBall seconds x] ++ advance seconds xs

-------------------------------------
-- | advance a single ball's simulation 
-- | the given number of seconds
-------------------------------------
updateBall :: Float -> BallState -> BallState
updateBall seconds ball = 
    applyForces forces Ball { 
        pos = pos', 
        vel = vel', 
        acc = (0, 0), 
        rad = rad ball, 
        col = col ball }
    where 
        forces = [gravity, wind, friction (vel ball), drag (rad ball) (vel')]
        vel'   = limit (add (vel ball) (acc ball)) (maxSpeed)
        pos'   = add (pos ball) (vel ball)  
          
-------------------------------------
-- | apply a force vector
-------------------------------------
applyForces :: [Vector2D] -> BallState -> BallState
applyForces forces state = Ball { 
    pos = pos state, 
    vel = vel state, 
    acc = foldl (add) (acc state) (map (applyMass (rad state)) (forces)),
    rad = rad state, 
    col = col state }
    
-------------------------------------
-- | apply a mass to a force
-------------------------------------
applyMass :: Float -> Vector2D -> Vector2D 
applyMass mass force = divide force mass
    
-------------------------------------
-- | gravity defined as a vector
-------------------------------------
gravity :: Vector2D; gravity = (0, -0.64)

-------------------------------------
-- | wind defined as a vector
-------------------------------------
wind :: Vector2D; wind = (0.01, 0)

-------------------------------------
-- | the friction of a circle with 
-- | the given velocity
-------------------------------------
friction :: Vector2D -> Vector2D
friction vel = Linear.scale (Linear.normalize (Linear.scale (vel) (-1))) (fricCo * normal)

fricCo :: Float; fricCo = 0.01

-------------------------------------
-- | the drag of a circle with the 
-- | given radius
-------------------------------------
drag :: Float -> Vector2D -> Vector2D
drag area vel = 
    Linear.scale (normalize (Linear.scale (vel) (-1))) -- drag vector
          (dragCo * (magnitude vel) * (magnitude vel)) -- drag magnitude

dragCo :: Float; dragCo = 0.1
normal :: Float; normal = 1
