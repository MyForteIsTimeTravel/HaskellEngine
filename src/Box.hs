--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Box (
    BoxState, 
    addBox, 
    pos, 
    vel, 
    acc, 
    rot, 
    rad, 
    col
) where

import Graphics.Gloss
import Linear
import Window

data BoxState = Box {
    pos :: Vector2D, -- the Box's position in 2D space
    vel :: Vector2D, -- the Box's velocity vector
    acc :: Vector2D, -- the Box's acceleration
    rot :: Float,    -- the Box's rotation
    rad :: Float,    -- the Box's radius, also functions as mass
    col :: Color     -- the Box's colour
} deriving Show

-------------------------------------
-- | add a Box to the state's list 
-- | of Boxs
-------------------------------------
addBox :: [BoxState] -> Int -> [BoxState]
addBox state size = state ++ [Box { 
    pos = makePos state size, 
    vel = Linear.rotate ((-10, 0)) (fromIntegral (length state)),
    acc = (-0.001, 0.01), 
    rot = 0.0,
    rad = 0.82 + (fromIntegral (length state)) * 0.02,
    col = makeColor ((fromIntegral ((length state))) / fromIntegral size) 
                    ((fromIntegral ((length state))) / fromIntegral size) 
                    ((fromIntegral ((length state))) / fromIntegral size) (0.4)}]

makePos :: [BoxState] -> Int -> Vector2D
makePos state size = (
     (((fromIntegral ((length state))) / fromIntegral size) / fromIntegral width),  -- x
     (((fromIntegral ((length state))) / fromIntegral size) / fromIntegral height)  -- y
     )