module Config where

-- =========================================
-- Константы, параметры
-- =========================================

defaultPopulation :: Int
defaultPopulation = 1000

drawScreenWidth :: Float
drawScreenWidth = 950.0

drawScreenHeight :: Float
drawScreenHeight = 384.0

-- | Ширина экрана.
screenWidth :: Int
screenWidth =  883

-- | Высота экрана.
screenHeight :: Int
screenHeight = 584

-- | Положение верхнего края экрана.
screenUp :: Float
screenUp = fromIntegral screenHeight / 2

-- | Положение нижнего края экрана.
screenDown :: Float
screenDown = - fromIntegral screenHeight / 2

-- | Положение правого края экрана.
screenRight :: Float
screenRight = fromIntegral screenWidth / 2

-- | Положение левого края экрана.
screenLeft :: Float
screenLeft = - fromIntegral screenWidth / 2
