{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Quest
  ( Quest (..),
    questId,
    questName,
    questDescription,
    questReward,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import GHC.Generics

data Quest = Quest
  { _questId :: Int,
    _questName :: String,
    _questDescription :: String,
    _questReward :: Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''Quest

instance FromJSON Quest