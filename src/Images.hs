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
  Just upRightPanel1'           <- loadJuicyPNG "images/upRightPanel1.png"
  Just upRightPanel2'           <- loadJuicyPNG "images/upRightPanel2.png"
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
  Just pondsInfo'               <- loadJuicyPNG "images/ponds_info.png"
  Just bancrotFrame'            <- loadJuicyPNG "images/bancrot_frame.png"
  Just restartButton'           <- loadJuicyPNG "images/restart_button.png"
  Just blankFrame'              <- loadJuicyPNG "images/blank_frame.png"
  Just mustBuy'                 <- loadJuicyPNG "images/must_buy.png"
  Just statsInfo'               <- loadJuicyPNG "images/stats_info.png"
  return Images
    { imageBackground         = scale  2.0  2.0  background'
    , emptyEnableSimpleButton = scale  0.4  0.2  emptyEnableSimpleButton'
    , emptyUnableSimpleButton = scale  0.4  0.2  emptyUnableSimpleButton'
    , pondpic                 = scale  0.13 0.13 pondpic'
    , piranha                 = scale  0.13 0.13 piranha'
    , rightPanel              = scale  1    1    rightPanel' 
    , upRightPanel1           = scale  1.2  1.2  upRightPanel1' 
    , upRightPanel2           = scale  1.2  1.2  upRightPanel2' 
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
    , pondsInfo               = scale  1.1  1.0  pondsInfo'
    , bancrotFrame            = scale  2.0  2.0  bancrotFrame'
    , restartButton           = scale  0.4  0.4  restartButton'
    , blankFrame              = scale  0.4  0.4  blankFrame'
    , mustBuy                 = scale  0.3  0.3  mustBuy'
    , statsInfo               = scale  0.5  0.5  statsInfo'
    }

