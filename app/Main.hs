module Main where

import FishFarm
import Images

main :: IO ()
main = do
   images <- loadImages
   run images
