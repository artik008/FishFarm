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
  , upRightPanel1           :: Picture
  , upRightPanel2           :: Picture
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
  , pondsInfo               :: Picture
  , restartButton           :: Picture
  , bancrotFrame            :: Picture
  , blankFrame              :: Picture
  , mustBuy                 :: Picture
  , statsInfo               :: Picture
  }

data Actions = SetUnable 
             | SetEnable 
             | SetVisible 
             | SetUnvisible 
             | Start 
             | Next
             | Restart
             | AutoSell
             | Bigger String
             | Smaller String
             | ChangeFish Int
             | SellFish Int
             | BuyFish Int
             | NoAction
  deriving (Eq)


-- * Модель
data Model = Model
  { ponds        :: [Pond]         -- ^ Пруды
  , startimg     :: Images         -- ^ Изображения для отрисовки интерфейса
  , startpop     :: Int            -- ^ Изначальное число рыб в одном пруду
  , gui          :: GUI       
  , contract     :: Contract       -- ^ Выбранный контракт
  , capital      :: Float          -- ^ Капитал фермы
  , forage       :: Float          -- ^ Количество запасённого корма
  , week         :: Int            -- ^ Текущая неделя
  , conditions   :: Conditions     -- ^ Коэффициенты
  , comPriceList :: (Bool, [Fish]) -- ^ Список цен на рыбу
  , stats        :: (Bool, Statistic)
  } deriving (Generic)

data FishType = Perch | CatFish | Crucian | Piranha | Herring | Pike | Shrimp | Shark 
                deriving(Show, Eq)

-- * Рыба
data Fish = Fish 
  { ftype     :: FishType  -- ^ Вид рыбы
  , priceSell :: Int       -- ^ Цена продажи
  , priceBuy  :: Int       -- ^ Цена покупки
  }

-- * Пруд
data Pond = Pond
  { number     :: Int      -- ^ Номер пруда
  , fishType   :: FishType -- ^ Вид рыбы в пруде
  , adult      :: Float    -- ^ Количество взрослых особей
  , young      :: Float    -- ^ Количество молодых особей
  , picture    :: Picture  
  , fishPrice  :: Int      -- ^ Цена продажи рыбы в пруду
  , fishBuy    :: Int      -- ^ Цена покупки рыбы в пруду
  }
  deriving (Show)

-- * Контракт
data Contract = Contract
  { duration     :: Int  -- ^ Длительность контракта
  , fishWeek     :: Int  -- ^ Количество рыбы, которое нужно продавать в неделю
  , lostFish     :: Int  -- ^ Сколько рыбы осталось продать на текущей неделе
  , fishBuyPrice :: Int  -- ^ Цена продажи рыбы по контракту
  , forageWeek   :: Int  -- ^ Количество корма, которое ферма покупает в неделю
  , foragePrice  :: Int  -- ^ Цена за один килограмм корма
  , fine         :: Int  -- ^ Штраф за каждый килограмм рыбы, который ферма не смогла продать за неделю по контракту
  }
  deriving (Show)

-- * Условия моделирования
data Conditions = Conditions
  { probability :: Float
  , fertility   :: Float -- ^ Коэффициент рождаемости
  , survival    :: Float -- ^ Коэффициент выживаемости
  , mortality   :: Float -- ^ Коэффициент смертности
  }

data Statistic = Statistic
  { boughtFish    :: Float
  , boughtCapital :: Float
  , selledFish    :: Float
  , selledCapital :: Float
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