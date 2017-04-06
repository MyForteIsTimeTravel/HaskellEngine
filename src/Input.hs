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
import Entity

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
handle (EventKey (MouseButton rightButton) bs _ pos) state = 
    state { entities = if bs == Down then map (mouseImpulse pos 256) (entities state) else (entities state) }

--handle (EventKey (MouseButton leftButton) bs _ pos) state = 
--    state { entities = (entities state) ++ mouseSpawn pos }

-- | blast impulse from mouse as it moves across the screen
handle (EventMotion pos) state = 
    if onScreen pos then state { entities = map (mouseSeek pos) (entities state) }
    else state
    
-- | default, don't do anything
handle _ state = state

-- | blast impulse from mouse
mouseImpulse :: Vector2D -> Float -> EntityState -> EntityState
mouseImpulse origin range entity = 
    entity { acc = applyImpulse (origin) (range) (pos entity) (acc entity) }

-- | attract towards mouse
mouseAttraction :: Vector2D -> Float -> EntityState -> EntityState
mouseAttraction origin range entity = 
    entity { acc = applyAttraction (origin) (range) (pos entity) (acc entity) }
    
-- | seek steer towards mouse
mouseSeek :: Vector2D -> EntityState -> EntityState
mouseSeek mouse entity = entity { tgt = mouse }

-- | spawn an entity on a mouse click
mouseSpawn :: Vector2D -> [EntityState]
mouseSpawn mouse = [entityAt mouse]