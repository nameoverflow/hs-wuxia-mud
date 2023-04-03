module Game.Message where

import Game.Entity
import qualified Data.Text as T

data PlayerAction
  = Go Direction
  | Attack CharId
  | ApplySkill SkillId
  | Use T.Text
  | Say T.Text
  | Other T.Text
  deriving (Show, Eq)

data ActionResp
  = MoveMsg Room
  | AttackMsg Character
  | SkillMsg Skill Character Character
  | CombatNormalMsg T.Text T.Text Move
  | UseItemMsg Item
  | SayMsg T.Text
  | DialogueMsg CharId T.Text

