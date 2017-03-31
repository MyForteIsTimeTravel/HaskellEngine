--------------------------------------------------------------
--------------------------------------------------------------
--
--  http://andrew.gibiansky.com/blog/haskell/haskell-gloss/
--
--  Graphics and Simulation in haskell
--  Ryan Needham
--
--------------------------------------------------------------
--------------------------------------------------------------
module Main (main) where

import Graphics.Gloss.Interface.Pure.Game

import Simulation
import Collision
import Rendering
import Window
import Linear
import Input

-------------------------------------
-- Main
-------------------------------------
-- | frame rate
fps :: Int
fps = 60

-- | run simulation on state
update :: Float -> [BallState] -> [BallState]
update seconds = ballsToTheWall . advance seconds

main :: IO ()
main = play window background fps (initialState count) render handleEvents update
