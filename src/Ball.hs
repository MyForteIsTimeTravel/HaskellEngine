--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Ball (
    BallState, 
    addBall, 
    looseBall, 
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

data BallState = Ball {
    pos :: Vector2D, -- the ball's position in 2D space
    vel :: Vector2D, -- the ball's velocity vector
    acc :: Vector2D, -- the ball's acceleration
    rot :: Float,    -- the ball's rotation
    rad :: Float,    -- the ball's radius, also functions as mass
    col :: Color     -- the ball's colour
} deriving Show

-------------------------------------
-- | add a ball to the state's list 
-- | of balls
-------------------------------------
addBall :: [BallState] -> Int -> [BallState]
addBall state size = state ++ [Ball { 
    pos = makePos state size, 
    vel = Linear.rotate ((-10, 0)) (fromIntegral (length state)),
    acc = (-0.001, 0.01), 
    rot = 0.0,
    rad = 4 + ((fromIntegral (length state)) * 0.2),
    col = makeColor ((fromIntegral ((length state))) / fromIntegral size) 
                    ((fromIntegral ((length state))) / fromIntegral size) 
                    ((fromIntegral ((length state))) / fromIntegral size) (0.4)}]

-------------------------------------
-- | remove a ball from the state if
-- | it contains any
-------------------------------------
looseBall :: [BallState] -> [BallState]
looseBall state = if not (null state) then tail state else state

makePos :: [BallState] -> Int -> Vector2D
makePos state size = (
     (((fromIntegral ((length state))) / fromIntegral size) / fromIntegral width),  -- x
     (((fromIntegral ((length state))) / fromIntegral size) / fromIntegral height)  -- y
     )