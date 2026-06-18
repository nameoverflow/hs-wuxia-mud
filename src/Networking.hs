{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Networking where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Game.CharacterCreation
import Game.Message

data NetEvent
  = RequestCharacterCreationConfig
  | Login
      { username :: Text,
        password :: Text,
        creation :: Maybe CharacterCreationChoice
      }
  | Disconnect
  | NetPlayerAction PlayerAction
  deriving (Show, Eq, Generic)

instance FromJSON NetEvent
instance ToJSON NetEvent
