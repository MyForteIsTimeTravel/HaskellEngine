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
update :: Float -> [BallState] -> [BallState]
update seconds = checkCollisions . advance seconds

-------------------------------------
-- | main
-------------------------------------
main :: IO ()
main = play                 -- gloss game initialiser
    window                  -- render target
    background              -- clear colour
    fps                     -- frame rate
    (initialState count)    -- initial engine state
    render                  -- draw engine state
    handleEvents            -- handle user events
    update                  -- update function
