module Ponds where


import Graphics.Gloss.Interface.Pure.Game
import Models

initPond :: Int -> Images -> FishType -> Int -> Pond
initPond num img ft pop = 
   Pond { number = num 
         , fishType   = ft
         , population = pop
         , picture = case ft of
            Piranha -> piranha img
            _       -> piranha img
         }

drawPond :: (Float, Float) -> Picture -> Pond -> Picture
drawPond (x,y) pic pnd = pictures ([translate x y pic, translate (x + 10) (y + 10) (picture pnd)])