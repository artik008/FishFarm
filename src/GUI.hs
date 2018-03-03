{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module GUI where

import Images()
import Models
import Ponds
import Config
import Graphics.Gloss.Interface.Pure.Game


initGUI :: Images -> Int -> GUI
initGUI images  pnum = GUI
  { buttons = [
    Button { name    = Just "Start"
           , eImage  = emptyEnableSimpleButton images
           , uImage  = emptyUnableSimpleButton images
           , size    = (148.0, 16.0)
           , bpos    = (450.0, -380.0)
           , bttext  = (16, 5)
           , enable  = True
           , visible = True
           , action  = Start
           }
  ] 
  ++ (choiseButtons images) ++ (choiseContractButtons images) 
  ++ (changeFishButtons images pnum)
  , frames = [ Frame (rightPanel images) (450.0, 0.0)
             , Frame (upRightPanel images) (450.0, 0.0)
             ]
  , labels = settingsLabels ++ defaultInfoLables

  }

changeFishButtons :: Images -> Int -> [Button]
changeFishButtons _ 0 = [] 
changeFishButtons img pnum = (changeFishButtons img (pnum-1)) 
  ++ [newchangeFishButton img pnum]

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
    newpos = if ppnum <= 4 then ((ppnum*(drawScreenWidth/(5.0))) - (screenRight*1.6), -100.0 - (screenUp))
                          else ((ppnum-4.0)*(drawScreenWidth/(5.0)) - (screenRight*1.6), 350.0 - (screenUp))

checkButtonPair :: (Float, Float) -> String -> Images -> [Button]
checkButtonPair pos s Images{..}= [
    choiseBtnTemp {eImage = biggerpic, bpos  = sumpos pos (80.0, 0.0), action = Bigger s}
  , choiseBtnTemp {eImage = smallerpic, bpos = sumpos pos (-80.0, 0.0), action = Smaller s}
  ]
  where
    sumpos (a, b) (c, d) = (a + c, b + d)

choiseButtons :: Images -> [Button]
choiseButtons img = 
      (checkButtonPair (450.0, stt - 3*th) "capital" img)
   ++ (checkButtonPair (350.0, stt - 5*th - 1*sh) "ponds" img)
   ++ (checkButtonPair (570.0, stt - 5*th - 1*sh) "population" img)
   ++ (checkButtonPair (450.0, stt - 12*th - 6*sh) "probability" img)
  where
    stt = 390.0
    th  = 30.0
    sh  = 50.0

choiseContractButtons :: Images -> [Button]
choiseContractButtons img =
      (checkButtonPair (460.0, stt - 7*th - 2*sh) "duration" img)
   ++ (checkButtonPair (330.0, stt - 8*th - 3*sh) "fishweek" img)
   ++ (checkButtonPair (560.0, stt - 8*th - 3*sh) "forageweek" img)
   ++ (checkButtonPair (330.0, stt - 9*th - 4*sh) "fishprice" img)
   ++ (checkButtonPair (560.0, stt - 9*th - 4*sh) "forageprice" img)
   ++ (checkButtonPair (450.0, stt - 10*th - 5*sh) "fine" img)
  where
    stt = 390.0
    th  = 30.0
    sh  = 50.0

settingsLabels :: [Label]
settingsLabels = 
             [ Label "Model settings"    (330.0, stt) (0.3, 0.3) red True
             , Label "Start capital"     (370.0, stt - 2*th) (0.25, 0.25) black True
             , Label "Ponds settings"    (350.0, stt - 3*th - 1*sh) (0.25, 0.25) black True
             , Label "number of ponds"   (260.0, stt - 4*th - 1*sh) (0.15, 0.15) black True
             , Label "start population"  (500.0, stt - 4*th - 1*sh) (0.15, 0.15) black True
             , Label "Contract settings" (330.0, stt - 5*th - 2*sh) (0.25, 0.25) black True
             , Label "Contract duration" (350.0, stt - 6*th - 2*sh) (0.2, 0.2) black True
             , Label "fish/week"         (285.0, stt - 7*th - 3*sh) (0.15, 0.15) black True
             , Label "forage/week"       (505.0, stt - 7*th - 3*sh) (0.15, 0.15) black True
             , Label "fish price"        (280.0, stt - 8*th - 4*sh) (0.15, 0.15) black True
             , Label "forage price"      (500.0, stt - 8*th - 4*sh) (0.15, 0.15) black True
             , Label "Fine/kg"           (400.0, stt - 9*th - 5*sh) (0.2, 0.2) black True
             , Label "Adverse conditions probability" (270.0, stt - 11*th - 6*sh) (0.2, 0.2) black True
             ]
  where
    stt = 380.0
    th  = 30.0
    sh  = 50.0

processLabels :: [Label]
processLabels = 
             [ Label "Model information" (320.0, stt) (0.3, 0.3) red True
             , Label "Farm capital"      (370.0, stt - 2*th) (0.25, 0.25) black True
             , Label "Ponds"             (420.0, stt - 3*th - 1*sh) (0.25, 0.25) black True
             , Label "number of ponds"   (260.0, stt - 4*th - 1*sh) (0.15, 0.15) black True
             , Label "total population"  (500.0, stt - 4*th - 1*sh) (0.15, 0.15) black True
             , Label "Contract info"     (350.0, stt - 5*th - 2*sh) (0.25, 0.25) black True
             , Label "Contract duration" (350.0, stt - 6*th - 2*sh) (0.2, 0.2) black True
             , Label "fish/week"         (285.0, stt - 7*th - 3*sh) (0.15, 0.15) black True
             , Label "forage/week"       (505.0, stt - 7*th - 3*sh) (0.15, 0.15) black True
             , Label "fish price"        (280.0, stt - 8*th - 4*sh) (0.15, 0.15) black True
             , Label "forage price"      (500.0, stt - 8*th - 4*sh) (0.15, 0.15) black True
             , Label "Fine/kg"           (400.0, stt - 9*th - 5*sh) (0.2, 0.2) black True
             , Label "Adverse conditions probability" (270.0, stt - 11*th - 6*sh) (0.2, 0.2) black True
             ]
  where
    stt = 380.0
    th  = 30.0
    sh  = 50.0

defaultInfoLables :: [Label]
defaultInfoLables = 
  [  Label "500.0"  (420.0, stt - 3*th) (0.2, 0.2) black True
   , Label "4"    (350.0, stt - 5*th - 1*sh) (0.15, 0.15) black True
   , Label "1000" (550.0, stt - 5*th - 1*sh) (0.15, 0.15) black True
   , Label "6"    (450.0, stt - 7*th - 2*sh) (0.2, 0.2) black True
   , Label "10"   (320.0, stt - 8*th - 3*sh) (0.15, 0.15) black True
   , Label "10"   (550.0, stt - 8*th - 3*sh) (0.15, 0.15) black True
   , Label "50"   (320.0, stt - 9*th - 4*sh) (0.15, 0.15) black True
   , Label "30"   (550.0, stt - 9*th - 4*sh) (0.15, 0.15) black True
   , Label "1"    (450.0, stt - 10*th - 5*sh) (0.2, 0.2) black True
   , Label "1 %"  (420.0, stt - 12*th - 6*sh) (0.2, 0.2) black True
  ]
  where
    stt = 380.0
    th  = 30.0
    sh  = 50.0

updateInfoLables :: Model -> Actions -> [Label]
updateInfoLables Model{..} act = 
  [  Label (show capital)  (420.0, stt - 3*th) (0.2, 0.2) black True
   , Label (show $ length ponds) (350.0, stt - 5*th - 1*sh) (0.15, 0.15) black True
   , Label (show $ checkpop) (550.0, stt - 5*th - 1*sh) (0.15, 0.15) black True
   , Label (show $ duration contract)    (450.0, stt - 7*th - 2*sh) (0.2, 0.2) black True
   , Label (show $ fishWeek contract)   (320.0, stt - 8*th - 3*sh) (0.15, 0.15) black True
   , Label (show $ forageWeek contract)   (550.0, stt - 8*th - 3*sh) (0.15, 0.15) black True
   , Label (show $ fishPrice contract)   (320.0, stt - 9*th - 4*sh) (0.15, 0.15) black True
   , Label (show $ foragePrice contract)   (550.0, stt - 9*th - 4*sh) (0.15, 0.15) black True
   , Label (show $ fine contract)    (450.0, stt - 10*th - 5*sh) (0.2, 0.2) black True
   , Label ((takeWhile (\x -> x /= '.') (show $ 100*(probability conditions))) ++ " %")  (420.0, stt - 12*th - 6*sh) (0.2, 0.2) black True
  ]
  where
    checkpop = case act of
      Start -> sum(map population ponds)
      _     -> case checkStat (buttons gui) of
        Start -> startpop 
        _     -> sum(map population ponds)
    stt = 380.0
    th  = 30.0
    sh  = 50.0

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
   , size    = (48.0, 48.0)
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
        Just n  -> [translate (x-tx) (y-ty) $ scale 0.15 0.15 (color white (text n))]
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
drawGUI GUI{..} = pictures ((map drawFrame frames) ++ (map drawButton buttons) ++ (map drawLabel labels))

checkButtons :: Model -> Images -> (Float, Float) -> Model
checkButtons m img coords = case isButton (buttons (gui m)) coords of
  Just btn -> makeAction m img btn
  Nothing  -> m

isButton :: [Button] -> (Float, Float) -> Maybe Button
isButton [] _ = Nothing
isButton (b: bs) (x,y)
  | ((bx+bsx)>x) && ((bx-bsx)<x) && ((by+bsy)>y) && ((by-bsy)<y) = Just b
  | otherwise = isButton bs (x,y) 
  where
    (bx, by)   = bpos b
    (bsx, bsy) = size b 

makeAction :: Model -> Images -> Button -> Model
makeAction m img b = m {
    ponds      = case action b of
      Bigger ts  -> ponds (makeBigger ts m)
      Smaller ts -> ponds (makeSmaller ts m)
      ChangeFish num -> map (changeFishType img num) (ponds m)
      _ -> ponds m
  , startpop   = case action b of
      Bigger ts  -> startpop (makeBigger ts m)
      Smaller ts -> startpop (makeSmaller ts m)
      _ -> startpop m
  , gui = (gui m) {
            buttons = case action b of
              SetUnable       -> (deleteB b bgm) ++ [b {enable = False}]
              SetEnable       -> (deleteB b bgm) ++ [b {enable = True, action = SetUnable}]
              SetUnvisible    -> (deleteB b bgm) ++ [b {visible = False}]
              SetVisible      -> (deleteB b bgm) ++ [b {visible = True, action = SetUnvisible}]
              Start           -> [b { name = Nothing
                                 , action = Next
                                 , eImage = (next img)
                                 }]
              Bigger "ponds"  -> if (length $ ponds m) < 8 then bgm ++ [newchangeFishButton img ((length $ ponds m)+1)] else bgm
              Smaller "ponds" -> if (length $ ponds m) > 0 then deleteB (newchangeFishButton img (length $ ponds m)) bgm else bgm
              _               -> bgm
          , labels = case action b of
              Start -> processLabels ++ (updateInfoLables m Start)
              Next  -> processLabels ++ (updateInfoLables m Next)
              Bigger  t -> settingsLabels ++ (updateInfoLables (makeBigger t m) NoAction)
              Smaller t -> settingsLabels ++ (updateInfoLables (makeSmaller t m) NoAction)
              _     -> lgm
             }
  , contract = case action b of
      Bigger ts  -> contract (makeBigger ts m)
      Smaller ts -> contract (makeSmaller ts m)
      _ -> contract m
  , capital  = case action b of
      Bigger ts  -> capital (makeBigger ts m)
      Smaller ts -> capital (makeSmaller ts m)
      _ -> capital m
  , conditions = case action b of
      Bigger ts  -> conditions (makeBigger ts m)
      Smaller ts -> conditions (makeSmaller ts m)
      _ -> conditions m
    }
  where
    bgm = buttons $ gui m
    lgm = labels $ gui m

makeBigger :: String -> Model -> Model
makeBigger t m = m {
    ponds      = case t of
      "ponds" -> if (length $ ponds m) < 8 then (ponds m) ++ [(initPond (length $ ponds m) (startimg m) Piranha (startpop m))] else (ponds m)
      "population" -> map bigerpop (ponds m)
      _ -> ponds m
  , startpop   = case t of
      "population" -> if (startpop m) < 5000 then (startpop m) + 100 else (startpop m) 
      _ -> startpop m
  , contract   = case t of
      "duration"    -> mc { duration    = if (duration mc) <= 21     then (duration mc) + 3 else (duration mc)} 
      "fishweek"    -> mc { fishWeek    = if (fishWeek mc) < 200    then (fishWeek mc) + 10 else (fishWeek mc)} 
      "fishprice"   -> mc { fishPrice   = if (fishPrice mc) < 300   then (fishPrice mc) + 10 else (fishPrice mc)} 
      "forageweek"  -> mc { forageWeek  = if (forageWeek mc) < 200  then (forageWeek mc) + 10 else (forageWeek mc)} 
      "forageprice" -> mc { foragePrice = if (foragePrice mc) < 300 then (foragePrice mc) + 10 else (foragePrice mc)} 
      "fine"        -> mc { fine        = if (fine mc) < 20         then (fine mc) + 1 else (fine mc)} 
      _ -> mc
  , capital = case t of
    "capital" -> if (capital m) < 5000.0 then (capital m) + 100.0 else (capital m)
    _  -> (capital m)
  , conditions = case t of
    "probability" -> if (probability $ conditions m) < 0.2 then (conditions m) { probability = (probability $ conditions m) + 0.01} else (conditions m)
    _ -> (conditions m)  
}
  where
    mc = contract m
    bigerpop p = p { population = if (population p) < 5000 then (population p) + 100 else (population p)} 

makeSmaller :: String -> Model -> Model
makeSmaller t m = m {
    ponds      = case t of
      "ponds" -> if (length $ ponds m) > 0 then take ((length (ponds m)) - 1) (ponds m) else (ponds m)
      "population" -> map smallerpop (ponds m)
      _ -> ponds m
  , startpop   = case t of
      "population" -> if (startpop m) >= 100 then (startpop m) - 100 else (startpop m) 
      _ -> startpop m
  , contract   = case t of
      "duration"    -> mc { duration    = if (duration mc) >= 9     then (duration mc) - 3 else (duration mc)} 
      "fishweek"    -> mc { fishWeek    = if (fishWeek mc) >= 10    then (fishWeek mc) - 10 else (fishWeek mc)} 
      "fishprice"   -> mc { fishPrice   = if (fishPrice mc) >= 10   then (fishPrice mc) - 10 else (fishPrice mc)} 
      "forageweek"  -> mc { forageWeek  = if (forageWeek mc) >= 10  then (forageWeek mc) - 10 else (forageWeek mc)} 
      "forageprice" -> mc { foragePrice = if (foragePrice mc) >= 10 then (foragePrice mc) - 10 else (foragePrice mc)} 
      "fine"        -> mc { fine        = if (fine mc) > 0         then (fine mc) - 1 else (fine mc)} 
      _ -> mc
  , capital = case t of
    "capital" -> if (capital m) > 100.0 then (capital m) - 100.0 else (capital m)
    _  -> (capital m)
  , conditions = case t of
    "probability" -> if (probability $ conditions m) > 0.00 then (conditions m) { probability = (probability $ conditions m) - 0.01} else (conditions m)
    _ -> (conditions m)  
}
  where
    mc = contract m
    smallerpop p = p { population = if (population p) >= 100 then (population p) - 100 else (population p)} 

changeFishType :: Images -> Int -> Pond -> Pond
changeFishType img num p
  | num == (number p) = p {fishType = nextTypex, picture = nextTypey}
  | otherwise = p
  where
    (nextTypex, nextTypey) = nextType
    nextType = case (fishType p) of
      Perch   -> (CatFish, catfishpic img)
      CatFish -> (Crucian, crucianpic img)
      Crucian -> (Piranha, piranha img)
      Piranha -> (Herring, herringpic img)
      Herring -> (Pike, pikepic img)
      Pike    -> (Shrimp, shrimppic img)
      Shrimp  -> (Shark, sharkpic img)
      Shark   -> (Perch, perchpic img)

deleteB :: Button -> [Button] -> [Button]
deleteB _ [] = []
deleteB btn (b:bs)
  | (bpos b) == (bpos btn) = bs
  | otherwise = [b] ++ (deleteB btn bs)