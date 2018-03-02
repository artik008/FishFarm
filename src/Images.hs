module Images where

import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game
import Models

-- | Загрузить изображения из файлов.
loadImages :: IO Images
loadImages = do
  Just background'              <- loadJuicyPNG "images/background.png"
  Just pondpic'                 <- loadJuicyPNG "images/pond.png"
  Just piranha'                 <- loadJuicyPNG "images/piranha.png"
  Just emptyUnableSimpleButton' <- loadJuicyPNG "images/empty_unable_button.png"
  Just emptyEnableSimpleButton' <- loadJuicyPNG "images/empty_enable_button.png"
  return Images
    { imageBackground         = scale  2.5  2.5 background'
    , emptyEnableSimpleButton = scale  0.2  0.2 emptyEnableSimpleButton'
    , emptyUnableSimpleButton = scale  0.2  0.2 emptyUnableSimpleButton'
    , pondpic                 = scale  0.2  0.2 pondpic'
    , piranha                 = scale  0.2  0.2 piranha'
    }

