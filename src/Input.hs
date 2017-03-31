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
import Physics
import Linear
import Window
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
    map (mouseImpulse pos 256) state

-- | blast impulse from mouse as it moves across the screen
handleEvents (EventMotion pos) state = 
    map (mouseImpulse pos 86) state
    
-- | default, don't do anything
handleEvents _ state = state

-- | blast impulse from mouse
mouseImpulse :: Vector2D -> Float -> BallState -> BallState
mouseImpulse origin range ball = ball { 
    pos = pos ball, 
    vel = vel ball, 
    acc = applyImpulse (origin) (range) (pos ball) (acc ball),
    rad = rad ball, 
    col = col ball }