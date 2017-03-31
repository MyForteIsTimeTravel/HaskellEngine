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
module Linear (Vector2D, add, sub, divide, scale, magnitude, normalize, distance, limit) where

type Vector2D = (Float, Float)

---- TO DO ----
-- setMag       - set the magnitude of a vector
-- limit        - limit the magnitude of a vector
-- heading      - the 2D heading of a vector expressed as an angle
-- rotate       - rotate a 2D vector by an angle
-- lerp         - linear interpolate to another vector
-- angleBetween - find the angle between two vectors
-- dot          - the dot product of two vectors
-- random2D()   - make a random 2D vector

-- | vector addition (+)
add :: Vector2D -> Vector2D -> Vector2D
add a b = (fst a + fst b, snd a + snd b)

-- | vector subtraction (-)
sub :: Vector2D -> Vector2D -> Vector2D
sub a b = (fst a - fst b, snd a - snd b)

-- | vector divide (/)
divide :: Vector2D -> Float -> Vector2D
divide vec d = if d > 0 then (fst vec / d, snd vec / d) else vec

-- | vector scaling (*)
scale :: Vector2D -> Float -> Vector2D
scale vec scl = (fst vec * scl, snd vec * scl)

-- | vector magnitude (|| v ||)
magnitude :: Vector2D -> Float
magnitude vec = sqrt (fst vec * fst vec + snd vec * snd vec)

-- | vector normalization (v / || v ||)
normalize :: Vector2D -> Vector2D
normalize vec = divide vec (magnitude vec)

-- | vector distance
distance :: Vector2D -> Vector2D -> Float
distance a b = magnitude (sub b a)

-- | set magnitude
withMagnitude :: Vector2D -> Float -> Vector2D
withMagnitude vec mag = scale (normalize (vec)) mag

-- | cap magnitude
limit :: Vector2D -> Float -> Vector2D
limit vec cap = if (magSqr vec) > cap * cap then scale (divide vec (sqrt (magSqr vec))) cap else vec

-- | square the magnitude
magSqr :: Vector2D -> Float
magSqr vec = (fst vec * fst vec + snd vec * snd vec)

-- | returns a random vector
--random :: Vector2D
--random = ()

-- | heading as an angle
--heading :: Vector2D -> Float
--heading vec = 

-- | rotate by an angle
--rotate :: Vector2D -> Angle -> Vector2D
--rotate vec theta =

-- | linearly interpolate between two vectors
--lerp :: Vector2D -> Vector2D -> Vector2D
--lerp a b =

-- | angle between two vectors
--between :: Vector2D -> Vector2D -> Float
--between a b =

-- | dot product
dot :: Vector2D -> Vector2D -> Float
dot a b = foldr (+) 0 (map multiply (zip [fst a, snd a] [fst b, snd b]))
    where multiply :: (Float, Float) -> Float 
          multiply a = fst a * snd a

    --public func dot   (other: Vec2) -> Float { return zip (self.array, other.array).map (*).reduce (0, {$0 + $1})}