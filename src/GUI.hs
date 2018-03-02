{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module GUI where

import Images()
import Models
import Graphics.Gloss.Interface.Pure.Game


initGUI :: Images -> GUI
initGUI Images{..} = GUI
  { buttons = [
    Button { name   = Just "OK"
           , eImage = emptyEnableSimpleButton
           , uImage = emptyUnableSimpleButton
           , size   = (75.0, 32.0)
           , pos    = (100.0, 100.0)
           , bscale = (1.0, 1.0)
           , enable = True
           , action = SetUnable
           }
  ]

  }

drawButton :: Button -> Picture
drawButton Button{..} = pictures ([translate x y (scale sx sy image)] ++ btext)
  where
    btext = case name of
        Just n  -> [translate (x-6) (y-5) $ scale 0.1 0.1 (color white (text n))]
        Nothing -> []
    (sx, sy) = bscale
    (x, y)   = pos
    image = if enable then eImage else uImage 

drawGUI :: GUI -> Picture
drawGUI GUI{..} = pictures (map drawButton buttons)

checkButtons :: Model -> (Float, Float) -> Model
checkButtons m coords = case isButton (buttons (gui m)) coords of
  Just btn -> makeAction m btn
  Nothing  -> m

isButton :: [Button] -> (Float, Float) -> Maybe Button
isButton [] _ = Nothing
isButton (b: bs) (x,y)
  | ((bx+bsx)>x) && ((bx-bsx)<x) && ((by+bsy)>y) && ((by-bsy)<y) = Just b
  | otherwise = isButton bs (x,y) 
  where
    (bx, by)   = pos b
    (bsx, bsy) = size b 

makeAction :: Model -> Button -> Model
makeAction m b = m {
    gui = (gui m) {
            buttons = (deleteB b bgm) ++ [actionToButton b] 
             }
    }
  where
    bgm = buttons $ gui m

deleteB :: Button -> [Button] -> [Button]
deleteB _ [] = []
deleteB btn (b:bs)
  | (pos b) == (pos btn) = bs
  | otherwise = [b] ++ (deleteB btn bs)

actionToButton :: Button -> Button
actionToButton btn = case action btn of
  SetUnable -> btn {enable = False, action = SetEnable}
  SetEnable -> btn {enable = True, action = SetUnable}
  -- _ -> btn 