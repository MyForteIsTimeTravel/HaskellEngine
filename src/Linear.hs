--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Linear (x, y, r, g, Vector2D,
               add, sub, divide, scale, 
               magnitude, normalize, distance, limit, rotate, withMagnitude,
               toDegrees, toRadians) where

-----------------------------------------
-- | alias declarations
-----------------------------------------
type Vector2D = (Float, Float)
x :: Vector2D -> Float; x v = fst v; y :: Vector2D -> Float; y v = snd v
r :: Vector2D -> Float; r v = fst v; g :: Vector2D -> Float; g v = snd v
-----------------------------------------
-- | Conversion
-----------------------------------------
toRadians :: Float -> Float; toRadians d = d * (pi / 180)
toDegrees :: Float -> Float; toDegrees r = r * (180 / pi)
toArray   :: Vector2D -> [Float]; toArray v = [(x v), (y v)]
-----------------------------------------
-- | vector addition (+)
-----------------------------------------
add :: Vector2D -> Vector2D -> Vector2D; add a b = ((x a) + (x b), (y a) + (y b))
-----------------------------------------
-- | vector subtraction (-)
-----------------------------------------
sub :: Vector2D -> Vector2D -> Vector2D; sub a b = ((x a) - (x b), (y a) - (y b))
-----------------------------------------
-- | vector divide (/)
-----------------------------------------
divide :: Vector2D -> Float -> Vector2D; divide vec d = if d > 0 then ((x vec) / d, (y vec) / d) else vec
-----------------------------------------
-- | vector scaling (*)
-----------------------------------------
scale :: Vector2D -> Float -> Vector2D; scale vec scl = ((x vec) * scl, (y vec) * scl)
-----------------------------------------
-- | vector magnitude (|| v ||)
-----------------------------------------
magnitude :: Vector2D -> Float; magnitude vec = sqrt ((x vec) * (x vec) + (y vec) * (y vec))
-----------------------------------------
-- | vector normalization (v / || v ||)
-----------------------------------------
normalize :: Vector2D -> Vector2D; normalize vec = vec `divide` (magnitude vec)
-----------------------------------------
-- | vector distance
-----------------------------------------
distance :: Vector2D -> Vector2D -> Float; distance a b = magnitude (sub b a)
-----------------------------------------
-- | set magnitude
-----------------------------------------
withMagnitude :: Vector2D -> Float -> Vector2D; withMagnitude vec mag = scale (normalize (vec)) mag
-----------------------------------------
-- | cap magnitude
-----------------------------------------
limit :: Vector2D -> Float -> Vector2D; limit v c = if (magSqr v) > c * c then scale (v `divide` (sqrt (magSqr v))) c else v
-----------------------------------------
-- | square the magnitude
-----------------------------------------
magSqr :: Vector2D -> Float; magSqr vec = ((x vec) * (x vec) + (y vec) * (y vec))
-----------------------------------------
-- | heading as an angle
-----------------------------------------
heading :: Vector2D -> Float; heading vec = atan2 (y vec) (x vec)
-----------------------------------------
-- | rotate by an angle
-----------------------------------------
rotate :: Vector2D -> Float -> Vector2D
rotate vec theta = 
    ((x vec) * cos (theta) - (y vec) * sin (theta), 
     (x vec) * sin (theta) + (y vec) * cos (theta))
-----------------------------------------
-- | angle between two vectors
-----------------------------------------
between :: Vector2D -> Vector2D -> Float
between a b = if   ((dot a b) / ((magnitude a) * (magnitude b)) <= -1) then pi
         else if   ((dot a b) / ((magnitude a) * (magnitude b)) >=  1) then 0
         else acos ((dot a b) / ((magnitude a) * (magnitude b)))
-----------------------------------------
-- | dot product
-----------------------------------------
dot :: Vector2D -> Vector2D -> Float
dot a b = foldr (+) 0 (map multiply (zip (toArray a) (toArray b)))
    where multiply :: (Float, Float) -> Float 
          multiply a = (x a) * (y a)