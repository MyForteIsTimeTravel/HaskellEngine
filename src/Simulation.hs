--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Simulation (
    GameState, paused,     tick, actors, forces,
    pause,     increase,   decrease,
    count,     initialize, advance) where

-------------------------------------
-- Simulate World
-------------------------------------

import Graphics.Gloss
import Physics
import Window
import Linear
import Ball

-------------------------------------
-- | the data model for a simulation
-------------------------------------
data GameState = Game {
    paused :: Bool,         -- should the simulation run
    tick   :: Float,        -- current simulation tick
    forces :: [Vector2D],   -- the current forces active
    actors :: [BallState]   -- actors in the scene
}

-------------------------------------
-- | init state
-------------------------------------
initialize :: GameState
initialize = Game {
    paused = False,
    tick   = 0,
    forces = [gravity, wind],
    actors = populate count
    }

-------------------------------------
-- | pause state
-------------------------------------
pause :: GameState -> GameState
pause state = state { paused = not (paused state) }
-------------------------------------
-- | add a ball
-------------------------------------
increase :: GameState -> GameState
increase state = state { actors = addBall (actors state) count }
-------------------------------------
-- | remove a ball
-------------------------------------
decrease :: GameState -> GameState
decrease state = state { actors = looseBall (actors state) }
-------------------------------------
-- | the number of balls in the 
-- | simualtion
-------------------------------------
count :: Int; count = 32
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
populate 1 = addBall [] count 
populate n = addBall (populate (n - 1)) count
-------------------------------------
-- | advance the whole simulation the 
-- | given number of seconds
-------------------------------------
advance :: Float -> [Vector2D] -> [BallState] -> [BallState]
advance seconds forces []     = []
advance seconds forces [x]    = [updateBall seconds forces x]
advance seconds forces (x:xs) = [updateBall seconds forces x] ++ advance seconds forces xs
-------------------------------------
-- | advance a single ball's simulation 
-- | the given number of seconds
-------------------------------------
updateBall :: Float -> [Vector2D] -> BallState -> BallState
updateBall seconds forces ball = ball { 
        pos = (pos ball) `add` (vel ball), 
        vel = vel', 
        acc = applyForces (forces ++ frc) (0, 0) (rad ball),
        rot = heading vel' 
    } where 
        frc  = [friction (vel ball), drag (rad ball) (vel')]
        vel' = limit ((vel ball) `add` (acc ball)) (maxSpeed)