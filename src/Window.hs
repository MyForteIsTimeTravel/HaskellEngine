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
width  :: Int; width  = 1280 -- | width of render window
height :: Int; height = 640 -- | height of render window

-------------------------------------
-- | render target
-------------------------------------
window :: Display; window = InWindow "HaskellEngine" (width, height) (10, 10)

-------------------------------------
-- | clear colour
-------------------------------------
background :: Color; background = dark white