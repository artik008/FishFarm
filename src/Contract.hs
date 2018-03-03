{-# LANGUAGE RecordWildCards #-}
module Contract where


import Models
import Graphics.Gloss.Interface.Pure.Game

defaultContract :: Contract
defaultContract = Contract
  { duration    = 6
  , fishWeek    = 10
  , fishPrice   = 50
  , forageWeek  = 10
  , foragePrice = 30
  , fine        = 1
  }