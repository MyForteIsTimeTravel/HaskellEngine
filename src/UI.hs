--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module UI (UINode, makeNode, uipos, size, elm, loc, txt, UIElement, makeElement, label, title, dotdotdot) where

import Graphics.Gloss
import Window
import Linear

-----------------------------------------
-- | generic UI Node
-----------------------------------------
data UIElement = Element {
    loc :: Vector2D,
    txt :: [Char]
}

makeElement :: String -> Vector2D -> UIElement
makeElement t l = Element { txt = t, loc = l }

data UINode = Node {
    uipos :: Vector2D,
    size  :: Vector2D, -- should size be defined by number of elements?
    elm   :: [UIElement] -- head uses title (), tail uses label ()
}

makeNode :: Vector2D -> Vector2D -> [UIElement] -> UINode
makeNode p s e = Node { uipos = p, size = s, elm = e }

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
label s p = 
    Translate (x p) (y p) $ 
    Scale     (fromIntegral (width) * 0.0001)  (fromIntegral (height) * 0.00016)  $ 
    Color     (dark white)    $
    Text      (s)
    
-----------------------------------------
-- | render string at given position with
-- | default font
-----------------------------------------
title :: [Char] -> Vector2D -> Picture
title s pos = 
    Translate (x pos) (y pos) $ 
    Scale     (fromIntegral (width) * 0.00012)  (fromIntegral (height) * 0.00018)   $ 
    Color     (black)         $
    Text      (s)