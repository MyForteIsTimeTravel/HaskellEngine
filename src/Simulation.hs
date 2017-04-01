--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Simulation (
    GameState, paused, tick, actors, 
    BallState, pos, vel, acc, rad, col, 
    count, initialize, advance) where

-------------------------------------
-- Simulate World
-------------------------------------

import Graphics.Gloss
import Physics
import Window
import Linear

-------------------------------------
-- | the data model for a simulation
-------------------------------------
data GameState = Game {
    paused :: Bool,         -- should the simulation run
    tick   :: Float,        -- current simulation tick
    actors :: [BallState]   -- actors in the scene
}

data BallState = Ball {
    pos :: Vector2D, -- the ball's position in 2D space
    vel :: Vector2D, -- the ball's velocity vector
    acc :: Vector2D, -- the ball's acceleration
    rad :: Float,    -- the ball's radius, also functions as mass
    col :: Color     -- the ball's colour
} deriving Show

-------------------------------------
-- | init state
-------------------------------------
initialize :: GameState
initialize = Game {
    paused = False,
    tick   = 0,
    actors = populate count
    }

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
populate :: Int -> [BallState]
populate 0 = []
populate 1 = addBall []
populate n = addBall (populate (n - 1))

-------------------------------------
-- | add a ball to the state's list 
-- | of balls
-------------------------------------
addBall :: [BallState] -> [BallState]
addBall state = state ++ [Ball { 
    pos = makePos state, 
    vel = Linear.rotate ((-10, 0)) (fromIntegral (length state)),
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
    ball { 
        pos = pos', 
        vel = vel', 
        acc = applyForces forces (0, 0) (rad ball)} 
    where 
        forces = [gravity, wind, friction (vel ball), drag (rad ball) (vel')]
        vel'   = limit ((vel ball) `add` (acc ball)) (maxSpeed)
        pos'   = (pos ball) `add` (vel ball)  