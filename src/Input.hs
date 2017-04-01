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

-------------------------------------
-- Handle IO
-------------------------------------
-- | manage events from the keyboard and mouse
handleEvents :: Event -> GameState -> GameState

-- | on the r key being pressed, reset the simulation
handleEvents (EventKey (Char 'r') ks _ _) state = if ks == Down then initialize else state

 -- | on the r key being pressed, reset the simulation
handleEvents (EventKey (Char 'p') ks _ _) state = if ks == Down then state { paused = not (paused state) } else state

-- | on mouse click, blast a force through the simulation
handleEvents (EventKey (MouseButton leftButton) bs _ pos) state = 
    state { actors = if bs == Down then map (mouseImpulse pos 256) (actors state) else (actors state) }

-- | blast impulse from mouse as it moves across the screen
handleEvents (EventMotion pos) state = 
    state { paused = (paused state), tick = (tick state), actors = map (mouseAttraction pos 256) (actors state) }

-- | default, don't do anything
handleEvents _ state = state

-- | blast impulse from mouse
mouseImpulse :: Vector2D -> Float -> BallState -> BallState
mouseImpulse origin range ball = 
    ball { acc = applyImpulse (origin) (range) (pos ball) (acc ball) }

-- | blast impulse from mouse
mouseAttraction :: Vector2D -> Float -> BallState -> BallState
mouseAttraction origin range ball = 
    ball { acc = applyAttraction (origin) (range) (pos ball) (acc ball) }