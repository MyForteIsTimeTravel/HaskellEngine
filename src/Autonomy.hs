--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Autonomy (seek, flee, arrive, contain, flow, align, seperate) where

import FlowField
import Window
import Physics
import Linear
import Entity

---------------------------------------------------
-- | SEEK
-- |
-- | Returns the seek force for the given entity
---------------------------------------------------
seek :: EntityState -> Float -> Float -> Vector2D
seek agent maxSpeed maxForce = ((desired agent maxSpeed) `sub` (vel agent)) `limit` maxForce

desired :: EntityState -> Float -> Vector2D
desired agent maxSpeed = (((tgt agent) `sub` (pos agent)) `withMagnitude` maxSpeed)

---------------------------------------------------
-- | FLEE
-- |
-- | Returns the flee force for the given entity
---------------------------------------------------
flee :: EntityState -> Float -> Float -> Vector2D
flee agent maxSpeed maxForce = ((feared agent maxSpeed) `sub` (vel agent)) `limit` maxForce

feared :: EntityState -> Float -> Vector2D
feared agent maxSpeed = (((pos agent) `sub` (tgt agent)) `withMagnitude` maxSpeed)

---------------------------------------------------
-- | ARRIVE 
-- |
-- | Returns the arrive force for the given entity
-- -- !! BROKEN !! --
---------------------------------------------------
arrive :: EntityState -> Float -> Float -> Vector2D
arrive agent maxSpeed maxForce = 
    if distance (tgt agent)(pos agent) < 200 then 
        (((tgt agent) `sub` (pos agent)) `withMagnitude` (rangeMap (distance (tgt agent)(pos agent)) 0 200 0 maxSpeed)) 
        `sub` (vel agent) 
        `limit` maxForce
    else
        ((desired agent maxSpeed) `sub` (vel agent)) 
        `limit` maxForce
        
--------------------------------------------------
-- | CONTAIN
-- | returns the force necessary to contain the sprite
-- | within the window
--------------------------------------------------
contain :: EntityState -> Float -> Float -> Vector2D
contain agent maxSpeed maxForce = 
    -- bottom
    if y (pos agent) < (fromIntegral height) * (-0.4) then 
        (x (vel agent), maxSpeed) `limit` maxForce 
    else if y (pos agent) > (fromIntegral height) * (0.4) then 
        (x (vel agent), -maxSpeed) `limit` maxForce 
    else (0, 0)
    
--------------------------------------------------
-- | FLOW
-- | returns the force necessary to folow the given
-- | flow field
--------------------------------------------------
flow :: EntityState -> [FlowPoint] -> Float -> Float -> Vector2D
flow agent field maxSpeed maxForce = 
    (findFlow (pos agent) (field) `scale` maxSpeed)
    `sub` (vel agent)
    `limit` maxForce
    
--------------------------------------------------
-- | ALIGN
-- | 
-- | sheeple
--------------------------------------------------
align :: EntityState -> [EntityState] -> Float -> Float -> Vector2D
align agent agents maxSpeed maxForce = 
    ((normalize (average (map vel (nearestEntities 16 agent agents)))) `scale` maxSpeed)
    `sub` (vel agent)
    `limit` maxForce
    
--------------------------------------------------
-- | SEPERATE
-- | - not yet working
--------------------------------------------------
seperate :: EntityState -> [EntityState] -> Float -> Float -> Vector2D
seperate agent agents maxSpeed maxForce = 
    (vel agent) `sub` (normalize ((foldl (add) (0, 0) (map (repell agent) (agents)))
        `divide`
    (fromIntegral (length ((foldl (add) (0, 0) (map (repell agent) (agents)))))))
        `scale`
    maxSpeed)
        `limit`
    maxForce
    
    
repell :: EntityState -> EntityState -> Vector2D
repell a b = 
    if (distance (pos a) (pos b)) > 10 then
        (normalize ((pos a) `sub` (pos b))) `divide` (distance (pos a) (pos b))
    else (0, 0)