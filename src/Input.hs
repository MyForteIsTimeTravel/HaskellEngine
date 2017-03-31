--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Input (handleEvents) where

import Graphics.Gloss.Interface.Pure.Game
import Simulation
import Linear

-- | TO DO
-- event / command queue

-------------------------------------
-- Handle IO
-------------------------------------
-- | manage events from the keyboard and mouse
handleEvents :: Event -> [BallState] -> [BallState]

-- | on the r key being pressed, reset the simulation
handleEvents (EventKey (Char 'r') ks _ _) state = 
    if ks == Down then initialState count else state

-- | on mouse click, blast a force through the simulation
handleEvents (EventKey (MouseButton leftButton) bs _ pos) state = 
    if (fst pos) < 0 then map (applyForces [(8.0, 6.0)]) state 
    else map (applyForces [(-8.0, 6.0)]) state 
    
-- | default, don't do anything
handleEvents _ state = state