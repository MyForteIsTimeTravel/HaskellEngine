--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Window (fps, width, height, window, background, bottomLeftX, bottomY, onScreen) where

-------------------------------------
-- Window Data
-------------------------------------

import Graphics.Gloss
import Linear

-------------------------------------
-- | window parameters
-------------------------------------
fps    :: Int; fps    = 60   -- | frame rate
width  :: Int; width  = 1400 -- | width of render window
height :: Int; height = 800  -- | height of render window

-------------------------------------
-- | render target
-------------------------------------
window :: Display; window = InWindow "HaskellEngine" (width, height) (10, 10)

-------------------------------------
-- | clear colour
-------------------------------------
background :: Color; background = dark white 

-------------------------------------
-- | useful positions
-------------------------------------
bottomLeftX :: Float
bottomLeftX = (fromIntegral width) * (-0.5)
bottomY :: Float
bottomY = (fromIntegral height) * (-0.5)

-------------------------------------
-- | says if a point is on the screen
-------------------------------------
onScreen :: Vector2D -> Bool
onScreen point = 
    if (x point) > fromIntegral (width)  * 0.5    then False else
    if (x point) < fromIntegral (width)  * (-0.5) then False else
    if (y point) > fromIntegral (height) * 0.5    then False else
    if (y point) < fromIntegral (height) * (-0.5) then False else
    True