--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Autonomy (seek, flee, arrive, contain) where

import Window
import Physics
import Linear
import Entity

---------------------------------------------------
-- | Returns the seek force for the given entity
---------------------------------------------------
seek :: EntityState -> Float -> Float -> Vector2D
seek agent maxSpeed maxForce = ((desired agent maxSpeed) `sub` (vel agent)) `limit` maxForce

desired :: EntityState -> Float -> Vector2D
desired agent maxSpeed = (((tgt agent) `sub` (pos agent)) `withMagnitude` maxSpeed)
---------------------------------------------------
-- | Returns the flee force for the given entity
---------------------------------------------------
flee :: EntityState -> Float -> Float -> Vector2D
flee agent maxSpeed maxForce = ((feared agent maxSpeed) `sub` (vel agent)) `limit` maxForce

feared :: EntityState -> Float -> Vector2D
feared agent maxSpeed = (((pos agent) `sub` (tgt agent)) `withMagnitude` maxSpeed)
---------------------------------------------------
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
-- | keeps the sprite in the window vertically
--------------------------------------------------
contain :: EntityState -> Float -> Float -> Vector2D
contain agent maxSpeed maxForce = 
    -- bottom
    if y (pos agent) < (fromIntegral height) * (-0.4) then 
        (x (vel agent), maxSpeed) `limit` maxForce 
    else if y (pos agent) > (fromIntegral height) * (0.4) then 
        (x (vel agent), -maxSpeed) `limit` maxForce 
    else (0, 0)