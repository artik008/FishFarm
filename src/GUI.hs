{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module GUI where

import Images()
import Models
import Config
import Graphics.Gloss.Interface.Pure.Game


changeFishButtons :: Images -> Int -> [Button]
changeFishButtons _ 0 = [] 
changeFishButtons img pnum = (changeFishButtons img (pnum-1)) 
  ++ [newchangeFishButton img pnum]

sellFishButtons :: Images -> Int -> [Button]
sellFishButtons _ 0 = [] 
sellFishButtons img pnum = (sellFishButtons img (pnum-1)) 
  ++ [(newchangeFishButton img pnum) {
       name   = Just " Sell 1 kg"
     , action = SellFish (pnum - 1)
     }]

buyFishButtons :: Images -> Int -> [Button]
buyFishButtons _ 0 = [] 
buyFishButtons img pnum = (buyFishButtons img (pnum-1)) 
  ++ [nfb {
       name   = Just " Buy 1 kg"
     , bpos   = sumpos (bpos nfb) (if pnum <= 4 then (0, -40.0) else (0, 40.0))
     , action = BuyFish (pnum - 1)
     }]
  where
    nfb = newchangeFishButton img pnum
    sumpos (a, b) (c, d) = (a + c, b + d)

autoSellButton :: Images -> Button
autoSellButton img = Button { 
   name    = Just "Auto Sell"
 , eImage  = emptyEnableSimpleButton img
 , uImage  = emptyUnableSimpleButton img
 , size    = (150.0, 32.0)
 , bpos    = (450.0, -300.0)
 , bttext  = (30, 5)
 , enable  = True
 , visible = True
 , action  = AutoSell
 }

newchangeFishButton :: Images -> Int -> Button
newchangeFishButton img pnum = btn {bpos = newpos, action = ChangeFish (pnum - 1)}
  where
    btn = Button { 
             name    = Just "Change Fish"
           , eImage  = changeFishPic img
           , uImage  = Blank
           , size    = (74.0, 16.0)
           , bpos    = (0.0, 0.0)
           , bttext  = (60.0, 6.0)
           , enable  = True
           , visible = True
           , action  = ChangeFish 0
           }
    ppnum  = fromIntegral pnum
    newpos = if ppnum <= 4 then ((ppnum*(drawScreenWidth/(5.0))) - (screenRight*1.6), -80.0 - (screenUp))
                          else ((ppnum-4.0)*(drawScreenWidth/(5.0)) - (screenRight*1.6), 230.0 - (screenUp))

checkButtonPair :: (Float, Float) -> String -> Images -> Float -> [Button]
checkButtonPair pos s Images{..} offset = [
    choiseBtnTemp {eImage = biggerpic, bpos  = sumpos pos (80.0 + offset, 0.0), action = Bigger s}
  , choiseBtnTemp {eImage = smallerpic, bpos = sumpos pos (-80.0, 0.0), action = Smaller s}
  ]
  where
    sumpos (a, b) (c, d) = (a + c, b + d)

choiseButtons :: Images -> [Button]
choiseButtons img = 
      (checkButtonPair (430.0, 310) "capital" img 40)
   ++ (checkButtonPair (340.0, 180) "ponds" img 0)
   ++ (checkButtonPair (570.0, 180) "population" img 0)


choiseContractButtons :: Images -> [Button]
choiseContractButtons img =
      (checkButtonPair (450.0, 75) "duration" img 0)
   ++ (checkButtonPair (340.0, -10) "fishweek" img 0)
   ++ (checkButtonPair (570.0, -10) "forageweek" img 0)
   ++ (checkButtonPair (340.0, -110) "fishprice" img 0)
   ++ (checkButtonPair (570.0, -110) "forageprice" img 0)
   ++ (checkButtonPair (450.0, -205) "fine" img 0)


defaultInfoLables :: [Label]
defaultInfoLables = 
  [  Label "5000.0" (410.0, 320.0) (0.2, 0.2) black True
   , Label "4"     (330.0, 290.0) (0.15, 0.15) black True
   , Label "400"  (550.0, 290.0) (0.15, 0.15) black True
   , Label "15"     (430.0, 250.0) (0.2, 0.2) black True
   , Label "100"    (320.0, 200.0) (0.15, 0.15) black True
   , Label "400"    (550.0, 200.0) (0.15, 0.15) black True
   , Label "0"     (320.0, 150.0) (0.15, 0.15) black True
   , Label "10"    (550.0, 150.0) (0.15, 0.15) black True
   , Label "20"     (430.0, 100.0) (0.2, 0.2) black True
   , Label "1"     (80.0, 360.0) (0.2, 0.2) black True
  ]

updateInfoLables :: Model -> Actions -> [Label]
updateInfoLables Model{..} act = 
  [  Label (show capital)  (410.0, 302.0) (0.2, 0.2) black True
   , Label (show $ length ponds) (330.0, 172.0) (0.15, 0.15) black True
   , Label (takeWhile (\x -> x /= '.') (show $ checkpop)) (550.0, 172.0) (0.15, 0.15) black True
   , Label (show $ duration contract)    (430.0, 68) (0.2, 0.2) black True
   , Label (show $ lostFish contract)   (320.0, -18) (0.15, 0.15) black True
   , Label (show $ forageWeek contract)   (550.0, -18) (0.15, 0.15) black True
   , Label (show $ fishBuyPrice contract)   (330.0, -118) (0.15, 0.15) black True
   , Label (show $ foragePrice contract)   (550.0, -118) (0.15, 0.15) black True
   , Label (show $ fine contract)    (430.0, -213) (0.2, 0.2) black True
   , Label (show week)  (80.0, 355.0) (0.2, 0.2) black True
  ]
  where
    checkpop = case act of
      Start -> sum((map adult ponds) ++ (map young ponds))
      _     -> case checkStat (buttons gui) of
                 Start -> fromIntegral $ startpop 
                 _     -> sum((map adult ponds) ++ (map young ponds))


isButton :: [Button] -> (Float, Float) -> Maybe Button
isButton [] _ = Nothing
isButton (b: bs) (x,y)
  | ((bx+bsx)>x) && ((bx-bsx)<x) && ((by+bsy)>y) && ((by-bsy)<y) = Just b
  | otherwise = isButton bs (x,y) 
  where
    (bx, by)   = bpos b
    (bsx, bsy) = size b 

checkStat :: [Button] -> Actions
checkStat b = case isButton b (450.0, -380.0) of
  Just btn -> action btn
  Nothing  -> NoAction

choiseBtnTemp :: Button
choiseBtnTemp = 
  Button {
     name    = Nothing
   , eImage  = Blank
   , uImage  = Blank
   , size    = (40.0, 40.0)
   , bpos    = (0.0, 0.0)
   , bttext  = (16.0, 5.0)
   , enable  = True
   , visible = True
   , action  = NoAction
  }

drawButton :: Button -> Picture
drawButton Button{..} = if visible then pictures ([translate x y image] ++ btext) else Blank
  where
    btext = case name of
        Just n  -> [translate (x-tx) (y-ty) $ scale 0.13 0.13 (color white (text n))]
        Nothing -> []
    (tx, ty) = bttext
    (x, y)   = bpos
    image = if enable then eImage else uImage 

drawFrame :: Frame -> Picture
drawFrame Frame{..} = translate x y fpic
  where
    (x, y)   = fpos

drawLabel :: Label -> Picture
drawLabel Label{..} = translate x y $ scale sx sy (color lcolor (text ltext))
  where
    (x, y)   = lpos
    (sx, sy) = lscale

drawGUI :: GUI -> Picture
drawGUI GUI{..} = pictures ((map drawFrame frames) 
                            ++ (map drawButton buttons) 
                            ++ (map drawLabel labels))

drawStats :: Statistic -> Images -> Picture
drawStats st img = pictures $  [ translate (-80) 180 $ statsInfo img
                                   , translate (0) 250 $ scale 0.2 0.2 $ color black $ text $ takeWhile (/= '.') (show (selledFish st))
                                   , translate (0) 190 $ scale 0.2 0.2 $ color black $ text $ takeWhile (/= '.') (show (selledCapital st))
                                   , translate (0) 130 $ scale 0.2 0.2 $ color black $ text $ takeWhile (/= '.') (show (boughtFish st))
                                   , translate (0) 70 $ scale 0.2 0.2 $ color black $ text $ takeWhile (/= '.') (show (boughtCapital st))
                                   ]


deleteB :: Button -> [Button] -> [Button]
deleteB _ [] = []
deleteB btn (b:bs)
  | (bpos b) == (bpos btn) = bs
  | otherwise = [b] ++ (deleteB btn bs)