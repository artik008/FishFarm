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
  , rightPanel              :: Picture
  , upRightPanel            :: Picture
  , next                    :: Picture
  , biggerpic               :: Picture
  , smallerpic              :: Picture
  , changeFishPic           :: Picture
  , catfishpic              :: Picture
  , crucianpic              :: Picture
  , herringpic              :: Picture
  , pikepic                 :: Picture
  , shrimppic               :: Picture
  , sharkpic                :: Picture
  , perchpic                :: Picture
  }

data Actions = SetUnable 
             | SetEnable 
             | SetVisible 
             | SetUnvisible 
             | Start 
             | Next
             | Bigger String
             | Smaller String
             | ChangeFish Int
             | NoAction


-- | Farm Model
data Model = Model
  { ponds      :: [Pond]  -- ^ Пруды
  , startimg   :: Images
  , startpop   :: Int
  , gui        :: GUI
  , contract   :: Contract
  , capital    :: Float
  , conditions :: Conditions
  } deriving (Generic)

data FishType = Perch | CatFish | Crucian | Piranha | Herring | Pike | Shrimp | Shark

data Pond = Pond
  { number     :: Int
  , fishType   :: FishType
  , population :: Int
  , picture    :: Picture
  }

data Contract = Contract
  { duration    :: Int
  , fishWeek    :: Int
  , fishPrice   :: Int
  , forageWeek  :: Int
  , foragePrice :: Int
  , fine        :: Int
  }

data Conditions = Conditions
  { probability :: Float
  }

-- | GUI Model
data Button = Button
 { name    :: Maybe String
 , eImage  :: Picture
 , uImage  :: Picture
 , size    :: (Float, Float)
 , bpos    :: (Float, Float)
 , bttext  :: (Float, Float)
 , enable  :: Bool
 , visible :: Bool
 , action  :: Actions
 }

data Frame = Frame
 { fpic  :: Picture
 , fpos  :: (Float, Float)
 }


data Label = Label
 { ltext  :: String
 , lpos   :: (Float, Float)
 , lscale :: (Float, Float)
 , lcolor :: Color
 , lvis   :: Bool
 }

data GUI = GUI
  { buttons :: [Button]
  , frames  :: [Frame]
  , labels  :: [Label]
  } 