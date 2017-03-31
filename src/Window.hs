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
module Window (width, height, window, background) where

import Graphics.Gloss

-------------------------------------
-- Init Window
-------------------------------------

-- | frame rate
fps :: Int
fps = 60

-- | width of render window
width :: Int
width = 640

-- | height of render window
height :: Int
height = 320

-- | Window implementation
window :: Display
window = InWindow "gloss" (width, height) (10, 10)

-- | clear colour
background :: Color
background = dark white