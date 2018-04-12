module Ponds where


import Graphics.Gloss.Interface.Pure.Game
import Models

drawPond :: (Float, Float) -> Picture -> Pond -> Picture
drawPond (x,y) pic pnd = pictures ([translate x y pic, translate (x + 10) (y + 10) (picture pnd)])


drawPondsInfo :: [Pond] -> Images -> Picture
drawPondsInfo ps img = pictures $  [ translate (-510) 220 $ pondsInfo img
                                   , translate (-690) 380 $ scale 0.1 0.1 $ color red $ text "N | Fish type |  Adult  |  Young  |  Sell | Buy"
                                   ] ++ (map drawPondInfo ps)

drawPondInfo :: Pond -> Picture
drawPondInfo p = pictures [
                   translate (-690) h $ scale 0.1 0.1 $ color black $ text $ show ((number p) + 1)
                  ,translate (-660) h $ scale 0.1 0.1 $ color black $ text $ show (fishType p)
                  ,translate (-575) h $ scale 0.1 0.1 $ color black $ text $ (takeWhile (\x -> x /= '.') (show (adult p))) ++ (take 2 $ dropWhile (\x -> x /= '.') (show (adult p)))
                  ,translate (-495) h $ scale 0.1 0.1 $ color black $ text $ (takeWhile (\x -> x /= '.') (show (young p))) ++ (take 2 $ dropWhile (\x -> x /= '.') (show (young p)))
                  ,translate (-410) h $ scale 0.1 0.1 $ color black $ text $ show (fishPrice p)
                  ,translate (-370) h $ scale 0.1 0.1 $ color black $ text $ show (fishBuy p)
                 ]
  where
    h = fromIntegral $ 390 - ((number p) + 1)* 40