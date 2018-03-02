{-# LANGUAGE DeriveGeneric #-}
module Models where

import GHC.Generics
import Graphics.Gloss.Interface.Pure.Game

-- | Изображения объектов.
data Images = Images
  { imageBackground         :: Picture -- ^ Фон.
  , emptyEnableSimpleButton :: Picture
  , emptyUnableSimpleButton :: Picture
  , pondpic                 :: Picture
  , piranha                 :: Picture
  }

data Actions = SetUnable | SetEnable

-- | Игровая вселенная
data Model = Model
  { ponds      :: [Pond]  -- ^ Пруды
  , gui        :: GUI
  } deriving (Generic)

data FishType = Perch | CatFish | Crucian | Piranha | Herring | Pike | Shrimp

data Pond = Pond
  { number     :: Int
  , fishType   :: FishType
  , population :: Int
  , picture    :: Picture
  }

data Button = Button
 { name    :: Maybe String
 , eImage  :: Picture
 , uImage  :: Picture
 , size    :: (Float, Float)
 , pos     :: (Float, Float)
 , bscale  :: (Float, Float)
 , enable  :: Bool
 , action :: Actions
 }

data GUI = GUI
  { buttons :: [Button]
  } 