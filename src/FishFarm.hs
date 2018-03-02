module FishFarm where

import Control.Concurrent.STM
import System.Random
import System.Exit
import Graphics.Gloss.Interface.IO.Game
import Config
import Models
import GUI
import Ponds

run :: Images -> IO ()
run images = do
  g <- newStdGen
  initWorld <- atomically $ newTVar (initModel images)
  playIO display bgColor fps initWorld drawWorld (handleWorld g) updateWorld
  where
    display = FullScreen -- InWindow "Fish Farm" (screenWidth, screenHeight) (150, 150)
    bgColor = black      -- цвет фона
    fps     = 60         -- кол-во кадров в секунду

    drawWorld w = do
      m <- readTVarIO w
      return (drawModel images m)

    handleWorld _ (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
    handleWorld g e w = atomically $ do
      u <- readTVar w
      writeTVar w (handleModel g e u)
      return w

    updateWorld dt w = do
      atomically $ modifyTVar w (updateModel dt)
      return w

-- =========================================
-- Отрисовка игровой вселенной
-- =========================================

initModel :: Images -> Model
initModel images = Model
  { ponds = [ initPond 0 images Piranha defaultPopulation
            , initPond 1 images Piranha defaultPopulation
            -- , initPond 2 images Piranha defaultPopulation
            , initPond 3 images Piranha defaultPopulation
            -- , initPond 4 images Piranha defaultPopulation
            , initPond 5 images Piranha defaultPopulation
            -- , initPond 6 images Piranha defaultPopulation
            , initPond 7 images Piranha defaultPopulation
            ]
  , gui   = initGUI images
  }

updateModel :: Float -> Model -> Model
updateModel _ m = m

-- | Отобразить игровую вселенную.
drawModel :: Images -> Model -> Picture
drawModel images  m = pictures
  ([ drawBackground (imageBackground images)
   , pictures (map (\x -> drawPond (getPondCoords x) (pondpic images) x) (ponds m))
   , drawGUI (gui m)
  ]) 
  where
    l = fromIntegral $ length (ponds m)
    num pnd = fromIntegral $ number pnd
    getPondCoords p = if l <= 4 
      then (((num p)+1.0)*(drawScreenWidth/(5.0)) - (screenRight*1.6), 0.0 - (screenUp))
      else if (number p) < 4 then (((num p)+1.0)*(drawScreenWidth/(5.0)) - (screenRight*1.6), 0.0 - (screenUp))
                               else (((num p)-3.0)*(drawScreenWidth/(5.0)) - (screenRight*1.6), 200.0 - (screenUp))
--  ++ drawMaybe [drawBack (imageStat images) (tableback u)]


-- | Отобразить фон.
drawBackground :: Picture -> Picture
drawBackground image = translate 0 0 image

-- =========================================
-- Обработка событий
-- =========================================

-- | Обработчик событий игры.
handleModel :: StdGen -> Event -> Model -> Model
handleModel = handleUserAction 

-- | Обработка нажатий игрока
handleUserAction :: StdGen -> Event -> Model -> Model
handleUserAction _ (EventKey (MouseButton LeftButton) Down _ coords) m = checkButtons m coords
handleUserAction _ _ m = m
