--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Entity (
    EntityState, 
    addEntity, 
    looseEntity, 
    pos, 
    vel, 
    acc, 
    tgt,
    rot, 
    rad, 
    col
) where

import Graphics.Gloss
import Linear
import Window

data EntityState = Entity {
    pos :: Vector2D, -- position in 2D space
    vel :: Vector2D, -- velocity vector
    acc :: Vector2D, -- acceleration vector
    tgt :: Vector2D, -- ai target
    rot :: Float,    -- z rotation
    rad :: Float,    -- radius, also functions as mass (RELEVANT?)
    col :: Color     -- colour
} deriving Show

-------------------------------------
-- | add a ball to the state's list 
-- | of balls
-------------------------------------
addEntity :: [EntityState] -> Int -> [EntityState]
addEntity state size = state ++ [Entity { 
    pos = makePos state size, 
    vel = Linear.rotate ((-10, 0)) (fromIntegral (length state)),
    acc = (-0.001, 0.01), 
    tgt = (200, 0),
    rot = 0.0,
    rad = 4 + ((fromIntegral (length state)) * 0.2),
    col = makeColor ((fromIntegral ((length state))) / fromIntegral size) 
                    ((fromIntegral ((length state))) / fromIntegral size) 
                    ((fromIntegral ((length state))) / fromIntegral size) (0.4)}]

-------------------------------------
-- | remove a ball from the state if
-- | it contains any
-------------------------------------
looseEntity :: [EntityState] -> [EntityState]
looseEntity []     = []
looseEntity (x:xs) = xs 

makePos :: [EntityState] -> Int -> Vector2D
makePos state size = (
     (((fromIntegral ((length state))) / fromIntegral size) / fromIntegral width),  -- x
     (((fromIntegral ((length state))) / fromIntegral size) / fromIntegral height)  -- y
     )