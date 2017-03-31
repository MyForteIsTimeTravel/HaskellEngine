--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Window (fps, width, height, window, background) where

-------------------------------------
-- Window Data
-------------------------------------

import Graphics.Gloss

-------------------------------------
-- | window parameters
-------------------------------------
fps    :: Int; fps    = 60  -- | frame rate
width  :: Int; width  = 640 -- | width of render window
height :: Int; height = 320 -- | height of render window

-------------------------------------
-- | render target
-------------------------------------
window :: Display; window = InWindow "gloss" (width, height) (10, 10)

-------------------------------------
-- | clear colour
-------------------------------------
background :: Color; background = dark white