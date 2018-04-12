{-# LANGUAGE RecordWildCards #-}
module Inits where

import Models
import Config
import Fish
import GUI
import Graphics.Gloss.Interface.Pure.Game

initModel :: Images -> Model
initModel images = Model
  { ponds = [ initPond 0 images Piranha defaultPopulation
            , initPond 1 images Piranha defaultPopulation
            , initPond 2 images Piranha defaultPopulation
            , initPond 3 images Piranha defaultPopulation
            ]
  , startimg     = images
  , startpop     = defaultPopulation
  , gui          = initGUI images 4
  , contract     = initContract
  , capital      = 5000
  , forage       = 0
  , week         = 1
  , conditions   = Conditions 0.01 0.3 0.8 0.85
  , comPriceList = (False, priceList)
  , stats        = (False, Statistic 0 0 0 0)
  }

end :: Bool -> Images -> Model -> Model
end False img m = m {
  gui = (gui m) {
          buttons = [
            Button { name    = Just ""
                   , eImage  = restartButton img
                   , uImage  = restartButton img
                   , size    = (120.0, 32.0)
                   , bpos    = (0.0, -140.0)
                   , bttext  = (16, 5)
                   , enable  = True
                   , visible = True
                   , action  = Restart
                   }]
        , frames = (frames (gui m)) ++ [Frame (bancrotFrame img) (0,0)]
        }
}
end True img m = m {
  gui = (gui m) {
          buttons = [
            Button { name    = Just ""
                   , eImage  = restartButton img
                   , uImage  = restartButton img
                   , size    = (120.0, 32.0)
                   , bpos    = (0.0, -40.0)
                   , bttext  = (16, 5)
                   , enable  = True
                   , visible = True
                   , action  = Restart
                   }]
        , frames = (frames (gui m)) ++ [Frame (blankFrame img) (0,0)]
        , labels = (labels (gui m)) ++ [Label "Contract is end!" (-110, 50) (0.2, 0.2) red True
                                       ,Label ("Your total capital = " ++ ((takeWhile (\x -> x /= '.') countCapital) ++ (take 2 $ dropWhile (\x -> x /= '.') countCapital))) (-190, 10.0) (0.2, 0.2) black True]
        }
  }
  where
    countCapital = show ((capital m) + (sum (map (\x -> (adult x)*(fromIntegral $ fishPrice x)) (ponds m))))


initGUI :: Images -> Int -> GUI
initGUI images  pnum = GUI
  { buttons = [
    Button { name    = Just "Start"
           , eImage  = emptyEnableSimpleButton images
           , uImage  = emptyUnableSimpleButton images
           , size    = (150.0, 32.0)
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
             , Frame (upRightPanel1 images) (450.0, 0.0)
             ]
  , labels = defaultInfoLables

  }

initPond :: Int -> Images -> FishType -> Int -> Pond
initPond num img ft pop = 
   Pond { number      = num 
         , fishType   = ft
         , fishPrice  = 30
         , fishBuy    = 15
         , adult      = (fromIntegral pop)/2.0
         , young      = (fromIntegral pop)/2.0
         , picture    = piranha img
         }

initContract :: Contract
initContract = Contract
  { duration     = 15
  , fishWeek     = 100
  , lostFish     = 100
  , fishBuyPrice = 0
  , forageWeek   = 400
  , foragePrice  = 10
  , fine         = 20
  }