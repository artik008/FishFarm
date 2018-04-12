{-# LANGUAGE RecordWildCards #-}
module Updates where

import Models
import Fish
import System.Random

updateFarm :: StdGen -> Model -> Model
updateFarm g m = case (lostFish (contract m)) of
  0 -> m {
      ponds        = if (mod (week m) 3) == 0 then updPondsPrices newPriceList (updatePonds m) else (updatePonds m)
    , contract     = updateContract m
    , capital      = newCapital m
    , forage       = newForage m
    , week         = (week m) + 1
    , comPriceList = (fst (comPriceList m), if (mod (week m) 3) == 0 then newPriceList else snd (comPriceList m))
    , stats        = (fst (stats m), Statistic 0 0 0 0)
    }
  _ -> m
  where
    newPriceList = updatePrices g (snd (comPriceList m))


updatePonds :: Model -> [Pond]
updatePonds Model{..} = map updatePond ponds
  where
    updatePond p = p { adult      = k*((survival conditions)*(young p) + (mortality conditions)*(adult p))
                     , young      = k*((fertility conditions)*(adult p))
                     } 
    k = if capital >= (fromIntegral $ (forageWeek contract)*(foragePrice contract)) 
        then if (fromIntegral $ forageWeek contract) + forage >= (sum (map young ponds))/2 + (sum (map adult ponds))
             then 1
             else ((fromIntegral $ forageWeek contract) + forage)/((sum (map young ponds))/2 + (sum (map adult ponds)))
        else if (capital/(fromIntegral $ foragePrice contract)) + forage >= (sum (map young ponds))/2 + (sum (map adult ponds))
             then 1
             else ((capital/(fromIntegral $ foragePrice contract)) + forage)/((sum (map young ponds))/2 + (sum (map adult ponds)))

updateContract :: Model -> Contract
updateContract Model{..} = contract {lostFish = fishWeek contract}

newCapital :: Model -> Float
newCapital Model{..} = if capital > (fromIntegral $ (forageWeek contract)*(foragePrice contract))
                       then capital - (fromIntegral $ (forageWeek contract)*(foragePrice contract))
                       else 0

newForage :: Model -> Float
newForage Model{..} = if capital > (fromIntegral $ (forageWeek contract)*(foragePrice contract) )
                      then if (fromIntegral $ forageWeek contract) + forage > (sum (map young ponds))/2 + (sum (map adult ponds))
                           then ((fromIntegral $ forageWeek contract) + forage) - (sum (map young ponds))/2 - (sum (map adult ponds))
                           else 0
                      else if (capital/(fromIntegral $ foragePrice contract)) + forage > (sum (map young ponds))/2 + (sum (map adult ponds))
                           then (capital/(fromIntegral $ foragePrice contract)) + forage - (sum (map young ponds))/2 - (sum (map adult ponds))
                           else 0

updPondsPrices :: [Fish] -> [Pond] -> [Pond]
updPondsPrices _ [] = []
updPondsPrices pl (p:ps) = [updPondPrice pl p] ++ (updPondsPrices pl ps)

updPondPrice :: [Fish] -> Pond -> Pond
updPondPrice [] p = p
updPondPrice (pl:pls) p = if (ftype pl) == (fishType p) 
  then p{fishPrice = (priceSell pl), fishBuy = (priceBuy pl)}  
  else updPondPrice pls p





