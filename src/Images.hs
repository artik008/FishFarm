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
  Just rightPanel'              <- loadJuicyJPG "images/rightPanel.jpg"
  Just upRightPanel'            <- loadJuicyPNG "images/upRightPanel.png"
  Just next'                    <- loadJuicyPNG "images/next.png"
  Just biggerpic'               <- loadJuicyPNG "images/bigger.png"
  Just smallerpic'              <- loadJuicyPNG "images/smaller.png"
  Just changeFishPic'           <- loadJuicyPNG "images/empty_enable_button.png"
  Just catfishpic'              <- loadJuicyPNG "images/catfishpic.png"
  Just crucianpic'              <- loadJuicyPNG "images/crucianpic.png"
  Just herringpic'              <- loadJuicyPNG "images/herringpic.png"
  Just pikepic'                 <- loadJuicyPNG "images/pikepic.png"
  Just shrimppic'               <- loadJuicyPNG "images/shrimppic.png"
  Just sharkpic'                <- loadJuicyPNG "images/sharkpic.png"
  Just perchpic'                <- loadJuicyPNG "images/perchpic.png"
  return Images
    { imageBackground         = scale  1.5  1.5  background'
    , emptyEnableSimpleButton = scale  0.4  0.2  emptyEnableSimpleButton'
    , emptyUnableSimpleButton = scale  0.4  0.2  emptyUnableSimpleButton'
    , pondpic                 = scale  0.13 0.13 pondpic'
    , piranha                 = scale  0.13 0.13 piranha'
    , rightPanel              = scale  1    1    rightPanel' 
    , upRightPanel            = scale  1.8  1.8  upRightPanel' 
    , next                    = scale  0.5  0.3  next' 
    , biggerpic               = scale  0.5  0.5  biggerpic' 
    , smallerpic              = scale  0.5  0.5  smallerpic' 
    , changeFishPic           = scale  0.2  0.1  changeFishPic' 
    , catfishpic              = scale  0.23 0.23 catfishpic'
    , crucianpic              = scale  0.23 0.23 crucianpic'
    , herringpic              = scale  0.23 0.23 herringpic'
    , pikepic                 = scale  0.25 0.25 pikepic'
    , shrimppic               = scale  0.13 0.13 shrimppic'
    , sharkpic                = scale  0.02 0.02 sharkpic'
    , perchpic                = scale  0.23 0.23 perchpic'
    }

