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
module Input (handleEvents) where

import Graphics.Gloss.Interface.Pure.Game
import Simulation
import Linear

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

-- | move all balls to the mouse position
ballsToMouse :: Vector2D -> [BallState] -> [BallState]
ballsToMouse loc []     = []
ballsToMouse loc [x]    = [ballToMouse loc x]
ballsToMouse loc (x:xs) = [ballToMouse loc x] ++ ballsToMouse loc xs

ballToMouse :: Vector2D -> BallState -> BallState
ballToMouse loc ball = ball { 
    pos = pos ball, 
    vel = Linear.scale (Linear.normalize (Linear.sub (loc) (pos ball))) (0.5), 
    acc = acc ball, 
    rad = rad ball, 
    col = col ball }