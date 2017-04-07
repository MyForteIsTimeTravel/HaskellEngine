--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Simulation (
    GameState, paused,     tick, entities, forces, flowfield, interface, makeInterface,
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
import UI

-------------------------------------
-- | the data model for a simulation
-------------------------------------
data GameState = Game {
    paused    :: Bool,          -- should the simulation run
    tick      :: Float,         -- current simulation tick
    forces    :: [Vector2D],    -- the current forces active
    flowfield :: [FlowPoint],   -- flow field of the environment
    entities  :: [EntityState], -- actors in the scene
    interface :: [UINode]       -- the simulation's UI
}

-------------------------------------
-- | init state
-------------------------------------
initialize :: GameState
initialize = Game {
    paused    = False,
    tick      = 0,
    forces    = [gravity, wind],
    flowfield = makeField 29 29,
    entities  = populate count,
    interface = []
}

makeInterface :: GameState -> [UINode]
makeInterface state = [
   -- entityUI (head (entities state)), 
    simulationUI state
    ]

-------------------------------------
-- | return an entities UI Node
-------------------------------------
entityUI :: EntityState -> UINode
entityUI entity = makeNode p s e
    where 
        p = ((-0.39 * fromIntegral width), (-0.195 * fromIntegral height))
        s = ((0.2 * fromIntegral width), (0.1 * fromIntegral height)) 
        e = [makeElement "Hello 1" ((-0.480 * fromIntegral width), (-0.260 * fromIntegral height)),
             makeElement "Hello 2" ((-0.480 * fromIntegral width), (-0.260 * fromIntegral height))]

-------------------------------------
-- | return the simulation's UI Node
-------------------------------------
simulationUI :: GameState -> UINode
simulationUI state = makeNode p s e
    where 
        p = ((-0.39 * fromIntegral width), (-0.39 * fromIntegral height))
        s = ((0.2 * fromIntegral width), (0.08 * fromIntegral height))
        e = [
        
            makeElement             
                    ("active forces : : " ++ show (length (forces state)))
                    ((-0.480 * fromIntegral width), (-0.410 * fromIntegral height)),
        
            makeElement             
                    ("entities : : " ++ show (length (entities state)))
                    ((-0.480 * fromIntegral width), (-0.440 * fromIntegral height)),
                    
             makeElement             
                    ("simulation tick : : " ++ show (tick state))
                    ((-0.480 * fromIntegral width), (-0.380 * fromIntegral height)),
                    
             makeElement             
                    (if (paused state) then "PAUSED" else ("RUNNING" ++ dotdotdot (tick state)))
                    ((-0.480 * fromIntegral width), (-0.350 * fromIntegral height))]

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
count :: Int; count = 64
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
advance :: Float -> GameState -> [EntityState] -> [EntityState]
advance seconds state []     = []
advance seconds state [x]    = [updateEntity seconds state x]
advance seconds state (x:xs) = [updateEntity seconds state x] ++ advance seconds state xs
-------------------------------------
-- | advance a single ball's simulation 
-- | the gyiven number of seconds
-------------------------------------
updateEntity :: Float -> GameState -> EntityState -> EntityState
updateEntity seconds state entity = entity { 
        pos = (pos entity) `add` (vel entity), 
        vel = vel', 
        acc = applyForces (frc ++ (forces state)) (0, 0) (rad entity),
        rot = heading (vel entity) 
    } where 
        frc  = [
                (seek entity maxSpeed maxForce),
                (flow entity (flowfield state) maxSpeed maxForce),
                --(align entity (entities state) maxSpeed maxForce),
                --(seperate entity (entities state) maxSpeed maxForce),
                (contain entity maxSpeed maxForce), 
                (drag (rad entity) (vel')),  
                (friction (vel entity))
                ]
        vel' = limit ((vel entity) `add` (acc entity)) (maxSpeed)
        
outputEntity :: EntityState -> IO()
outputEntity e = putStrLn(show e)