{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Networking where

import Control.Applicative (empty, pure)
import Data.Aeson
import qualified Data.HashMap.Lazy as HML (lookup)
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import GameState
import Game.Message

data NetEvent
  = Login
      { username :: Text,
        password :: Text
      }
  | Disconnect
  | NetPlayerAction PlayerAction
  deriving (Show, Eq, Generic)

instance FromJSON NetEvent
instance ToJSON NetEvent

