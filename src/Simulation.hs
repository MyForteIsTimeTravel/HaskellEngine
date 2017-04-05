--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Simulation (
    GameState, paused,     tick, entities, forces,
    pause,     increase,   decrease,
    count,     initialize, advance) where

-------------------------------------
-- Simulate World
-------------------------------------

import Graphics.Gloss
import FlowField
import Autonomy
import Physics
import Window
import Linear
import Entity

-------------------------------------
-- | the data model for a simulation
-------------------------------------
data GameState = Game {
    paused    :: Bool,         -- should the simulation run
    tick      :: Float,        -- current simulation tick
    forces    :: [Vector2D],   -- the current forces active
    flowfield :: [[Vector2D]], -- flow field of the environment
    entities  :: [EntityState]   -- actors in the scene
}

-------------------------------------
-- | init state
-------------------------------------
initialize :: GameState
initialize = Game {
    paused    = False,
    tick      = 0,
    forces    = [gravity, wind],
    flowfield = flowField 10,
    entities  = populate count
}

-------------------------------------
-- | pause state
-------------------------------------
pause :: GameState -> GameState
pause state = state { paused = not (paused state) }
-------------------------------------
-- | add an entity
-------------------------------------
increase :: GameState -> GameState
increase state = state { entities = addEntity (entities state) count }
-------------------------------------
-- | remove an entity
-------------------------------------
decrease :: GameState -> GameState
decrease state = state { entities = looseEntity (entities state) }
-------------------------------------
-- | the number of initial entities in the 
-- | simualtion
-------------------------------------
count :: Int; count = 1
-------------------------------------
-- | the highest attainable speed in 
-- | the simulation
-------------------------------------
maxSpeed :: Float; maxSpeed = 12.0
-------------------------------------
-- | the highest attainable force in 
-- | the simulation
-------------------------------------
maxForce :: Float; maxForce = 4.0
-------------------------------------
-- | return a list of n random ball
-- | states
-------------------------------------
populate :: Int -> [EntityState]
populate 0 = []
populate 1 = addEntity [] count 
populate n = addEntity (populate (n - 1)) count
-------------------------------------
-- | advance the whole simulation the 
-- | given number of seconds
-------------------------------------
advance :: Float -> [Vector2D] -> [EntityState] -> [EntityState]
advance seconds forces []     = []
advance seconds forces [x]    = [updateEntity seconds forces x]
advance seconds forces (x:xs) = [updateEntity seconds forces x] ++ advance seconds forces xs
-------------------------------------
-- | advance a single ball's simulation 
-- | the gyiven number of seconds
-------------------------------------
updateEntity :: Float -> [Vector2D] -> EntityState -> EntityState
updateEntity seconds forces entity = entity { 
        pos = (pos entity) `add` (vel entity), 
        vel = vel', 
        acc = applyForces (forces ++ frc) (0, 0) (rad entity),
        rot = heading vel' 
    } where 
        frc  = [friction (vel entity), drag (rad entity) (vel'), (arrive entity maxSpeed maxForce), (contain entity maxSpeed maxForce)]
        vel' = limit ((vel entity) `add` (acc entity)) (maxSpeed)