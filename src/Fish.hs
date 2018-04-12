module Fish where

import Models
import System.Random

priceList :: [Fish]
priceList = [ Fish Perch   35 20
            , Fish CatFish 45 30
            , Fish Crucian 20 12
            , Fish Piranha 30 15
            , Fish Herring 15 11
            , Fish Pike    40 25
            , Fish Shrimp  50 40
            , Fish Shark   60 50
            ]

updatePrices :: StdGen -> [Fish] -> [Fish]
updatePrices _ [] = []
updatePrices g (p: ps) = [p { priceSell = (priceSell p) + a
                              , priceBuy  = (priceBuy p) + b}] ++ (updatePrices nextg ps)
  where
    (g1, g2) = split g
    (a, nextg) = randomR (-5, 5) g1 
    (b, _) = randomR (-5, 5) g2 
