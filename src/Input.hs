--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Input (handle) where

import Graphics.Gloss.Interface.Pure.Game
import Simulation
import Physics
import Linear
import Window
import Ball

-------------------------------------
-- Handle IO
-------------------------------------
-- | manage events from the keyboard and mouse
handle :: Event -> GameState -> GameState

-- | Keys
handle (EventKey (Char 'r') ks _ _) state = if ks == Down then initialize else state
handle (EventKey (Char 'p') ks _ _) state = if ks == Down then pause state else state
handle (EventKey (Char 'x') ks _ _) state = increase state
handle (EventKey (Char 'z') ks _ _) state = decrease state

-- | on mouse click, blast a force through the simulation
handle (EventKey (MouseButton leftButton) bs _ pos) state = 
    state { actors = if bs == Down then map (mouseImpulse pos 256) (actors state) else (actors state) }

-- | blast impulse from mouse as it moves across the screen
handle (EventMotion pos) state = 
    state { paused = (paused state), tick = (tick state), actors = map (mouseAttraction pos 256) (actors state) }

-- | default, don't do anything
handle _ state = state

-- | blast impulse from mouse
mouseImpulse :: Vector2D -> Float -> BallState -> BallState
mouseImpulse origin range ball = 
    ball { acc = applyImpulse (origin) (range) (pos ball) (acc ball) }

-- | attract towards mouse
mouseAttraction :: Vector2D -> Float -> BallState -> BallState
mouseAttraction origin range ball = 
    ball { acc = applyAttraction (origin) (range) (pos ball) (acc ball) }