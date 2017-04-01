--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Simulation (BallState, pos, vel, acc, rad, col, count, initialState, advance) where

-------------------------------------
-- Simulate World
-------------------------------------

import Graphics.Gloss
import Physics
import Window
import Linear

-------------------------------------
-- | the data model for a ball
-------------------------------------

-- IMPLEMENT THROUGHOUT ENGINE
data GameState = Game {
    paused :: Bool,         -- should the simulation run
    tick   :: Float,        -- current simulation tick
    actors :: [BallState]   -- actors in the scene
}

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
count :: Int; count = 512

-------------------------------------
-- | the highest attainable speed in 
-- | the simulation
-------------------------------------
maxSpeed :: Float; maxSpeed = 16.0

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
    pos = makePos state, 
    vel = Linear.rotate ((-10, 0)) (fromIntegral (length state)),
    --vel = (0.0, 0.0),
    acc = (-0.001, 0.01), 
    rad = 0.82 + (fromIntegral (length state)) * 0.02,
    col = makeColor ((fromIntegral ((length state))) / fromIntegral count) 
                    ((fromIntegral ((length state))) / fromIntegral count) 
                    ((fromIntegral ((length state))) / fromIntegral count) (0.4)
}]

makePos :: [BallState] -> Vector2D
makePos state = (
     (((fromIntegral ((length state))) / fromIntegral count) / fromIntegral width),  -- x
     (((fromIntegral ((length state))) / fromIntegral count) / fromIntegral height)   -- y
     )

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
    Ball { 
        pos = pos', 
        vel = vel', 
        acc = applyForces forces (0, 0) (rad ball), 
        rad = rad ball, 
        col = col ball } 
    where 
        forces = [gravity, wind, friction (vel ball), drag (rad ball) (vel')]
        vel'   = limit ((vel ball) `add` (acc ball)) (maxSpeed)
        pos'   = (pos ball) `add` (vel ball)  