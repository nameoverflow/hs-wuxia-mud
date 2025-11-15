{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Message where

import Game.Entity
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson
import Relude (ToText(toText))
import Data.Text
import Data.HashMap as HM
import Control.Applicative ((<|>))

data PlayerAction
  = Go Direction
  | Talk CharId
  | Attack CharId
  | Perform SkillId
  | Use T.Text
  | Say T.Text
  | Other T.Text
  deriving (Show, Eq, Generic)

instance FromJSON PlayerAction where
  parseJSON = withObject "PlayerAction" $ \o ->
    (Go <$> o .: "go")
      <|> (Attack <$> o .: "attack")
      <|> (Perform <$> o .: "perform")
      <|> (Use <$> o .: "use")
      <|> (Say <$> o .: "say")
      <|> (Other <$> o .: "other")
      <|> (Talk <$> o .: "talk")


instance ToJSON PlayerAction where
  toJSON (Go dir) = object ["go" .= dir]
  toJSON (Attack charId) = object ["attack" .= charId]
  toJSON (Perform skillId) = object ["perform" .= skillId]
  toJSON (Use item) = object ["use" .= item]
  toJSON (Say msg) = object ["say" .= msg]
  toJSON (Other msg) = object ["other" .= msg]
  toJSON (Talk charId) = object ["talk" .= charId]

data ActionResp
  -- | dst room name
  = MoveMsg T.Text
  -- | room name, room desc, list of char names, list of exits
  | ViewMsg T.Text T.Text [T.Text] [Direction]
  -- | attacker, defender
  | AttackMsg T.Text T.Text
  -- | attacker, defender, skill desc
  | SkillMsg T.Text T.Text T.Text
  -- | attacker, defender, skill desc, damage
  | CombatNormalMsg T.Text T.Text T.Text Int
  -- | attacker, defender, win
  | CombatSettlementMsg T.Text T.Text Bool
  -- | applier, item desc
  | UseItemMsg T.Text T.Text
  -- | speaker, message
  | SayMsg T.Text T.Text
  -- | char name, message
  | DialogueMsg T.Text T.Text
  deriving (Show, Eq, Generic)

instance ToJSON ActionResp

type PlayerResp = (PlayerId, ActionResp)

formatResp :: ActionResp -> IO Text
formatResp r = return . toText $ Prelude.show r