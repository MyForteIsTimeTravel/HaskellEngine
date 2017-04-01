--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Physics (applyForces, applyImpulse, applyAttraction, gravity, wind, friction, drag) where

import Linear

-------------------------------------
-- | apply a force vector
-------------------------------------
applyForces :: [Vector2D] -> Vector2D -> Float -> Vector2D
applyForces forces acc rad = foldl (add) (acc) (map (applyMass (rad)) (forces))

-------------------------------------
-- | apply a mass to a force
-------------------------------------
applyMass :: Float -> Vector2D -> Vector2D 
applyMass mass force = divide force mass

-------------------------------------
-- | apply an impulse from a location with the given range
--      Vector2D o : position of impulse
--      Float    r : range of impulse
--      Vector2D p : position of object
--      Vector2D a : acceleration of object
--      Vector2D   : new acceleration
-------------------------------------
applyImpulse :: Vector2D -> Float -> Vector2D -> Vector2D -> Vector2D 
applyImpulse o r p a = if magnitude (p `sub` o) < r then (p `sub` o) else a

-------------------------------------
-- | apply an attraction from a location with the given range
--      Vector2D o : position of attraction
--      Float    r : range of attraction
--      Vector2D p : position of object
--      Vector2D a : acceleration of object
--      Vector2D   : new acceleration
-------------------------------------
applyAttraction :: Vector2D -> Float -> Vector2D -> Vector2D -> Vector2D 
applyAttraction o r p a = if magnitude (o `sub` p) < r then (o `sub` p) else a

-------------------------------------
-- | gravity defined as a vector
-------------------------------------
gravity :: Vector2D; gravity = (0, -0.64)

-------------------------------------
-- | wind defined as a vector
-------------------------------------
wind :: Vector2D; wind = (0.032, 0)

-------------------------------------
-- | the friction of a circle with 
-- | the given velocity
-------------------------------------
friction :: Vector2D -> Vector2D
friction vel = Linear.scale (Linear.normalize (Linear.scale (vel) (-1))) (fCo * normal)

fCo :: Float; fCo = 0.01
-------------------------------------
-- | the drag of a circle with the 
-- | given radius
-------------------------------------
drag :: Float -> Vector2D -> Vector2D
drag area vel = 
    Linear.scale (normalize (Linear.scale (vel) (-1))) -- drag vector
          (dCo * (magnitude vel) * (magnitude vel)) -- drag magnitude

dCo :: Float; dCo = 0.1
normal :: Float; normal = 1
