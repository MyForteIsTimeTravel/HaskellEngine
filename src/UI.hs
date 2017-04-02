--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module UI (label, title, dotdotdot) where

import Graphics.Gloss
import Linear

-----------------------------------------
-- | animated ellipsis
-----------------------------------------
dotdotdot :: Float -> [Char]
dotdotdot tick = take (1 + (round ((tick * 4)) `mod` 6)) (repeat '.')
-----------------------------------------
-- | render string at given position with
-- | default font
-----------------------------------------
label :: [Char] -> Vector2D -> Picture
label s pos = 
    Translate (x pos) (y pos) $ 
    Scale     (0.12)  (0.12)  $ 
    Color     (dark white)    $
    Text      (s)
-----------------------------------------
-- | render string at given position with
-- | default font
-----------------------------------------
title :: [Char] -> Vector2D -> Picture
title s pos = 
    Translate (x pos) (y pos) $ 
    Scale     (0.14) (0.14)   $ 
    Color     (black)         $
    Text      (s)