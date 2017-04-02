--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Main (main) where

-------------------------------------
-- main
-------------------------------------

import Graphics.Gloss.Interface.Pure.Game
import Simulation
import Collision
import Rendering
import Window
import Linear
import Input

-------------------------------------
-- | run simulation and collision 
-- | detection on engine state
-------------------------------------
update :: Float -> GameState -> GameState
update sec state = state { 
    tick   = if not (paused state) then (tick state) + 0.01 else (tick state),
    actors = if not (paused state) then checkCollisions (advance sec (forces state) (actors state)) else (actors state)
    }

-------------------------------------
-- | main
-------------------------------------
main :: IO ()
main = play    -- gloss game initialiser
    window     -- render target
    background -- clear colour
    fps        -- frame rate
    initialize -- initial engine state
    render     -- draw engine state
    handle     -- handle user events
    update     -- update function
