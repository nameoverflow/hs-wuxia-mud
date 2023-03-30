{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Networking where

import Control.Applicative (empty, pure)
import Data.Aeson
import qualified Data.HashMap.Lazy as HML (lookup)
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import GameState (PlayerAction)


data NetMessage
  = Look
  | Status
  | Item
  | Skill
  | Control PlayerControl
  deriving (Show, Eq, Generic)

instance Serialize NetMessage

instance FromJSON NetMessage

instance ToJSON NetMessage

data NetEvent
  = Login
      { userName :: Text,
        password :: Text
      }
  | Disconnect
  | Message NetMessage
  deriving (Show, Eq, Generic)

instance FromJSON NetEvent

instance Serialize NetEvent

instance ToJSON NetEvent

type NetInput = [NetEvent]
