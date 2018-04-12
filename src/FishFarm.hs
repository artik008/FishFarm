module FishFarm where

import Control.Concurrent.STM
import System.Random
import System.Exit
import Graphics.Gloss.Interface.IO.Game
import Config
import Models
import GUI
import Control
import Ponds
import Inits

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
      writeTVar w (handleModel g e images u)
      return w

    updateWorld dt w = do
      atomically $ modifyTVar w (updateModel dt images)
      return w


updateModel :: Float -> Images -> Model -> Model
updateModel _ img m = if (sum (map (truncate.adult) (ponds m)) == 0) && (lostFish (contract m) > 0) && ((capital m) < (fromIntegral $ (lostFish (contract m)) * (fine (contract m))))
  then end False img m
  else if (duration (contract m)) < (week m) 
       then end True img m
       else m {
    gui = (gui m) {
      labels = updateInfoLables m Next
    }
  , capital  = if (sum (map (truncate.adult) (ponds m)) == 0) && (lostFish (contract m) > 0) then (capital m) - (fromIntegral $ (lostFish (contract m)) * (fine (contract m))) else capital m
  , contract = (contract m) {
                 lostFish = if (sum (map (truncate.adult) (ponds m)) == 0) && (lostFish (contract m) > 0) then 0 else (lostFish (contract m))
               }
  }

-- | Отобразить игровую вселенную.
drawModel :: Images -> Model -> Picture
drawModel images  m = pictures
  ([ drawBackground (imageBackground images)
   , pictures (map (\x -> drawPond (getPondCoords x) (pondpic images) x) (ponds m))
   , if fst (stats m) then drawStats (snd (stats m)) images else Blank  
   , drawGUI (gui m)
   , if pondInf then drawPondsInfo (ponds m) images else Blank  
   ]) 
  where
    l               = fromIntegral $ length (ponds m)
    num pnd         = fromIntegral $ number pnd
    (pondInf, _)    = comPriceList m 
    getPondCoords p = if l <= 4 
      then (((num p)+1.0)*(drawScreenWidth/(5.0)) - (screenRight*1.6), 0.0 - (screenUp))
      else if (number p) < 4 then (((num p)+1.0)*(drawScreenWidth/(5.0)) - (screenRight*1.6), 0.0 - (screenUp))
                               else (((num p)-3.0)*(drawScreenWidth/(5.0)) - (screenRight*1.6), 150.0 - (screenUp))


-- | Отобразить фон.
drawBackground :: Picture -> Picture
drawBackground image = translate (-220) 0 image

-- =========================================
-- Обработка событий
-- =========================================

-- | Обработчик событий игры.
handleModel :: StdGen -> Event -> Images -> Model -> Model
handleModel = handleUserAction 

-- | Обработка нажатий игрока
handleUserAction :: StdGen -> Event -> Images -> Model -> Model
handleUserAction g (EventKey (MouseButton LeftButton) Down _ coords) img m = checkButtons g m img coords
handleUserAction _ (EventKey (Char 'p') Down _ _) _ m = 
  if (fst (comPriceList m) == False) 
  then m { comPriceList = (True, (snd $ (comPriceList m)))} 
  else m { comPriceList = (False, (snd $ (comPriceList m)))}
handleUserAction _ (EventKey (Char 's') Down _ _) _ m = 
  if (fst (stats m) == False) 
  then m { stats = (True, (snd $ (stats m)))} 
  else m { stats = (False, (snd $ (stats m)))}
handleUserAction _ _ _ m = m
