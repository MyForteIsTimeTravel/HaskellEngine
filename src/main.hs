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
update sec state 
    |     (paused state) = state
    | not (paused state) = state { 
            tick      = (tick state) + 0.01, 
            entities  = checkCollisions (advance sec state (entities state)),
            interface = makeInterface state
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
