{-# LANGUAGE RecordWildCards #-}
module Control where

import Models
import Updates
import Inits
import GUI
import System.Random hiding(next)

checkButtons :: StdGen -> Model -> Images -> (Float, Float) -> Model
checkButtons g m img coords = case isButton (buttons (gui m)) coords of
  Just btn -> makeAction g m img btn
  Nothing  -> m

makeAction :: StdGen -> Model -> Images -> Button -> Model
makeAction g m img b = if (action b) == Restart
  then initModel img
  else m {
    ponds      = case action b of
      Bigger ts      -> ponds (makeBigger ts m)
      Smaller ts     -> ponds (makeSmaller ts m)
      ChangeFish num -> map (changeFishType img num) (ponds m)
      BuyFish num    -> if (capital m) > (fromIntegral $ getPrice num (ponds m)) then map (buyFish (capital m) num) (ponds m) else ponds m
      SellFish num   -> if (getAdultCount num (ponds m)) >= 1 then map (sellFish num) (ponds m) else ponds m
      Next           -> ponds uF
      AutoSell       -> pondsAS
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
                                 , size  = (140.0, 30.0)
                                 }] ++ (sellFishButtons img (length $ ponds m)) ++ (buyFishButtons img (length $ ponds m)) ++ [autoSellButton img]
              Bigger "ponds"  -> if (length $ ponds m) < 8 then bgm ++ [newchangeFishButton img ((length $ ponds m)+1)] else bgm
              Smaller "ponds" -> if (length $ ponds m) > 1 then deleteB (newchangeFishButton img (length $ ponds m)) bgm else bgm
              _               -> bgm
          , labels = case action b of
              Start -> (updateInfoLables m Start)
              Next  -> (updateInfoLables m Next)
              Bigger  t -> (updateInfoLables (makeBigger t m) NoAction)
              Smaller t -> (updateInfoLables (makeSmaller t m) NoAction)
              _     -> lgm
          , frames = case action b of
              Start ->  [ Frame (rightPanel img) (450.0, 0.0)
                        , Frame (upRightPanel2 img) (450.0, 0.0)
                        ] 
              Next  ->  [ Frame (rightPanel img) (450.0, 0.0)
                        , Frame (upRightPanel2 img) (450.0, 0.0)
                        ]
                        ++ (if (lostFish (contract m) > 0) then [Frame (mustBuy img) (450.0, -240.0)] else [])
              AutoSell -> [ Frame (rightPanel img) (450.0, 0.0)
                        , Frame (upRightPanel2 img) (450.0, 0.0)
                        ]
              SellFish _ -> [ Frame (rightPanel img) (450.0, 0.0)
                        , Frame (upRightPanel2 img) (450.0, 0.0)
                        ]
              _ -> frames (gui m)
             }
  , contract = case action b of
      Bigger ts    -> contract (makeBigger ts m)
      Smaller ts   -> contract (makeSmaller ts m)
      SellFish num -> (contract m) { lostFish = (lostFish (contract m)) - (if ((getAdultCount num (ponds m)) >= 1) && ((lostFish (contract m)) > 0) then 1 else 0)}
      Next         -> contract uF
      AutoSell     -> contractAS
      _ -> contract m
  , capital  = case action b of
      Bigger ts    -> capital (makeBigger ts m)
      Smaller ts   -> capital (makeSmaller ts m)
      BuyFish num  -> if (capital m) > (fromIntegral $ getPrice num (ponds m)) then (capital m) - (fromIntegral $ getPrice num (ponds m)) else (capital m)
      SellFish num -> if (getAdultCount num (ponds m)) >= 1 then (capital m) + (fromIntegral $ getPriceSell num (ponds m)) else (capital m)  
      Next         -> capital uF
      AutoSell     -> capitalAS
      _ -> capital m
  , conditions = case action b of
      Bigger ts  -> conditions (makeBigger ts m)
      Smaller ts -> conditions (makeSmaller ts m)
      _ -> conditions m
  , forage = case action b of
      Next -> forage uF
      _    -> forage m
  , week = case action b of
      Next -> week uF
      _    -> week m
  , comPriceList = case action b of
      Next -> comPriceList uF
      _    -> comPriceList m
  , stats = (fst (stats m), (case action b of 
        Next -> snd $ stats uF
        AutoSell -> (snd (stats m)) {selledFish = (sum (map adult (ponds m))) - (sum (map adult pondsAS))
                              ,selledCapital = (capitalAS) - (capital m)}
        SellFish num -> (snd (stats m)) {selledFish = (selledFish $ snd (stats m)) + 1, selledCapital = (selledCapital $ snd (stats m)) + (fromIntegral $ if (getAdultCount num (ponds m)) >= 1 then getPriceSell num (ponds m) else 0) }
        BuyFish num -> (snd (stats m)) {boughtFish = (boughtFish $ snd (stats m)) + 1, boughtCapital = (boughtCapital $ snd (stats m)) + (fromIntegral $ if (capital m) > (fromIntegral $ getPrice num (ponds m)) then  getPrice num (ponds m) else 0) }
        _ -> snd (stats m)))
  }
  where
    (pondsAS, contractAS, capitalAS) = makeAutoSell (ponds m, contract m, capital m) 0
    bgm = buttons $ gui m
    lgm = labels $ gui m
    uF  = updateFarm g m

makeBigger :: String -> Model -> Model
makeBigger t m = m {
    ponds      = case t of
      "ponds" -> if (length $ ponds m) < 8 then (ponds m) ++ [(initPond (length $ ponds m) (startimg m) Piranha (startpop m))] else (ponds m)
      "population" -> map bigerpop (ponds m)
      _ -> ponds m
  , startpop   = case t of
      "population" -> if (startpop m) < 200 then (startpop m) + 10 else (startpop m) 
      _ -> startpop m
  , contract   = case t of
      "duration"    -> mc { duration     = if (duration mc) < 24     then (duration mc) + 3 else (duration mc)} 
      "fishweek"    -> mc { fishWeek     = if (fishWeek mc) < 800    then (fishWeek mc) + 10 else (fishWeek mc)
                          , lostFish     = if (fishWeek mc) < 800    then (fishWeek mc) + 10 else (fishWeek mc)} 
      "fishprice"   -> mc { fishBuyPrice = if (fishBuyPrice mc) < 10 then (fishBuyPrice mc) + 1 else (fishBuyPrice mc)} 
      "forageweek"  -> mc { forageWeek   = if (forageWeek mc) < 2000  then (forageWeek mc) + 100 else (forageWeek mc)} 
      "forageprice" -> mc { foragePrice  = if (foragePrice mc) < 50 then (foragePrice mc) + 5 else (foragePrice mc)} 
      "fine"        -> mc { fine         = if (fine mc) < 100         then (fine mc) + 5 else (fine mc)} 
      _ -> mc
  , capital = case t of
    "capital" -> if (capital m) < 15000.0 then (capital m) + 500.0 else (capital m)
    _  -> (capital m)
  , conditions = case t of
    "probability" -> if (probability $ conditions m) < 0.2 then (conditions m) { probability = (probability $ conditions m) + 0.01} else (conditions m)
    _ -> (conditions m)  
}
  where
    mc = contract m
    bigerpop p = p { adult = if (adult p) < 100 then (adult p) + 5 else (adult p) 
                   , young = if (young p) < 100 then (young p) + 5 else (young p)} 

makeSmaller :: String -> Model -> Model
makeSmaller t m = m {
    ponds      = case t of
      "ponds" -> if (length $ ponds m) > 1 then take ((length (ponds m)) - 1) (ponds m) else (ponds m)
      "population" -> map smallerpop (ponds m)
      _ -> ponds m
  , startpop   = case t of
      "population" -> if (startpop m) > 0 then (startpop m) - 10 else (startpop m) 
      _ -> startpop m
  , contract   = case t of
      "duration"    -> mc { duration     = if (duration mc) > 6     then (duration mc) - 3 else (duration mc)} 
      "fishweek"    -> mc { fishWeek     = if (fishWeek mc) > 0    then (fishWeek mc) - 10 else (fishWeek mc)
                          , lostFish     = if (fishWeek mc) > 0    then (fishWeek mc) - 10 else (fishWeek mc)} 
      "fishprice"   -> mc { fishBuyPrice = if (fishBuyPrice mc) > -10   then (fishBuyPrice mc) - 1 else (fishBuyPrice mc)} 
      "forageweek"  -> mc { forageWeek   = if (forageWeek mc) > 0  then (forageWeek mc) - 100 else (forageWeek mc)} 
      "forageprice" -> mc { foragePrice  = if (foragePrice mc) > 0 then (foragePrice mc) - 5 else (foragePrice mc)} 
      "fine"        -> mc { fine         = if (fine mc) > 0         then (fine mc) - 5 else (fine mc)} 
      _ -> mc
  , capital = case t of
    "capital" -> if (capital m) > 0.0 then (capital m) - 500.0 else (capital m)
    _  -> (capital m)
  , conditions = case t of
    "probability" -> if (probability $ conditions m) > 0.00 then (conditions m) { probability = (probability $ conditions m) - 0.01} else (conditions m)
    _ -> (conditions m)  
}
  where
    mc = contract m
    smallerpop p = p { adult = if (adult p) > 0 then (adult p) - 5 else (adult p) 
                     , young = if (young p) > 0 then (young p) - 5 else (young p)} 

buyFish :: Float -> Int -> Pond -> Pond
buyFish i num p
  | num == (number p) = if i >= (fromIntegral $ fishPrice p) then p {young = (young p) + 1} else p 
  | otherwise = p

sellFish :: Int -> Pond -> Pond
sellFish num p
  | num == (number p) = p {adult = (adult p) - 1} 
  | otherwise = p

getPrice :: Int -> [Pond] -> Int
getPrice _ [] = 0
getPrice num (p:ps) = if num == (number p) then (fishBuy p) else getPrice num ps

getPriceSell :: Int -> [Pond] -> Int
getPriceSell _ [] = 0
getPriceSell num (p:ps) = if num == (number p) then (fishPrice p) else getPriceSell num ps

getAdultCount :: Int -> [Pond] -> Float
getAdultCount _ [] = 0
getAdultCount num (p:ps) = if num == (number p) then (adult p) else getAdultCount num ps

changeFishType :: Images -> Int -> Pond -> Pond
changeFishType img num p
  | num == (number p) = p {fishType = nextTypex, picture = nextTypey, fishPrice = nextTypez, fishBuy = nextTypew}
  | otherwise = p
  where
    (nextTypex, nextTypey, nextTypez, nextTypew) = nextType
    nextType = case (fishType p) of
      Perch   -> (CatFish, catfishpic img, 45, 30)
      CatFish -> (Crucian, crucianpic img, 20, 12)
      Crucian -> (Piranha, piranha img, 30, 15)
      Piranha -> (Herring, herringpic img, 15, 11)
      Herring -> (Pike, pikepic img, 40, 25)
      Pike    -> (Shrimp, shrimppic img, 50, 40)
      Shrimp  -> (Shark, sharkpic img, 60, 50)
      Shark   -> (Perch, perchpic img, 35, 20)

makeAutoSell :: ([Pond], Contract, Float) -> Int -> ([Pond], Contract, Float)
makeAutoSell (p, cont, cap) num = if ((lostFish cont) == 0) || (sum (map (truncate.adult) p) == 0)  then (newPonds, newCont, newCap) else makeAutoSell (newPonds, newCont, newCap) newNum
  where
    newNum   = if num < ((length p)-1) then num + 1 else 0
    (newPonds, countFish, price) = halfOfPond p num (lostFish cont)
    newCont  = cont {lostFish = (lostFish cont) - countFish}
    newCap   = cap + (fromIntegral $ price * countFish)

halfOfPond :: [Pond] -> Int -> Int -> ([Pond], Int, Int)
halfOfPond [] _ _ = ([], 0, 0)
halfOfPond (p:ps) num lost = 
  if (number p) == num 
  then (if (truncate $ (adult p)/2.0) > lost 
        then ([p{adult = (adult p) - (fromIntegral $ truncate $ (adult p)/2.0) }] ++ ps, lost, fishPrice p)
        else (if (truncate $ (adult p)/2.0) == 0 
                     then if (adult p) < 1 then ([p] ++ ps, 0, 0) else ([p{adult = (adult p) - 1.0 }] ++ ps, 1, fishPrice p) 
                     else ([p{adult = (adult p) - (fromIntegral $ truncate $ (adult p)/2.0) }] ++ ps, (truncate $ (adult p)/2.0), fishPrice p)))
  else ([p]++ nponds, ncount, nprice)
  where
    (nponds, ncount, nprice) = halfOfPond ps num lost








