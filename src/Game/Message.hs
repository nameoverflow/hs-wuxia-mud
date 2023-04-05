{-# LANGUAGE DeriveGeneric #-}

module Game.Message where

import Game.Entity
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Relude (ToText(toText))
import Data.Text

data PlayerAction
  = Go Direction
  | Attack CharId
  | ApplySkill SkillId
  | Use T.Text
  | Say T.Text
  | Other T.Text
  deriving (Show, Eq, Generic)

instance FromJSON PlayerAction

data ActionResp
  = MoveMsg Room
  -- | attacker, defender
  | AttackMsg T.Text T.Text
  -- | attacker, defender, skill desc
  | SkillMsg T.Text T.Text T.Text
  -- | attacker, defender, skill desc, damage
  | CombatNormalMsg T.Text T.Text T.Text Int
  -- | attacker, defender
  | CombatSettlementMsg T.Text T.Text
  -- | applier, item desc
  | UseItemMsg T.Text T.Text
  -- | speaker, message
  | SayMsg T.Text T.Text
  -- | char name, message
  | DialogueMsg T.Text T.Text

  deriving (Show, Eq, Generic)

type PlayerResp = (PlayerId, ActionResp)

formatResp :: ActionResp -> IO Text
formatResp r = return . toText $ show r