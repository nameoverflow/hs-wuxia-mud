{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Entity where

import Control.Applicative ((<|>))
import Control.Lens (makeLenses, (&), (.~), (%~), (^.))
import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..), ToJSONKey (..), Value (..), object, withObject, (.:), (.:?), (.=))
-- import Data.Yaml (FromJSON, ParseException, decodeEither', decodeFileEither, withObject, (.:), (.:?))
-- import Data.Yaml.Aeson (FromJSON (..), Value (..))
-- import Data.Yaml.Parser (typeMismatch)

import Data.Aeson.Types
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import qualified Data.Set as S
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Utils

type ItemId = Text

type ArtId = Text

data ItemUse = LearnArtUse
  { _itemUseArt :: ArtId,
    _itemUseLevel :: Int,
    _itemUseConsume :: Bool,
    _itemUseMessage :: Maybe Text,
    _itemUseRepeatMessage :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, Serialize)

instance FromJSON ItemUse where
  parseJSON = withObject "ItemUse" $ \o -> do
    useType <- o .: "type"
    case useType of
      String "learn_art" -> do
        _itemUseArt <- o .: "art"
        _itemUseLevel <- o .:? "level" .!= 1
        _itemUseConsume <- o .:? "consume" .!= False
        _itemUseMessage <- o .:? "message"
        _itemUseRepeatMessage <- o .:? "repeat_message"
        pure LearnArtUse {..}
      _ -> fail "Invalid ItemUse type"

instance ToJSON ItemUse where
  toJSON LearnArtUse {..} =
    object
      [ "type" .= ("learn_art" :: Text),
        "art" .= _itemUseArt,
        "level" .= _itemUseLevel,
        "consume" .= _itemUseConsume,
        "message" .= _itemUseMessage,
        "repeat_message" .= _itemUseRepeatMessage
      ]

data Item = Item
  { _itemId :: ItemId,
    _itemName :: Text,
    _itemDesc :: Text,
    _itemUse :: Maybe ItemUse
  }
  deriving (Show, Eq, Ord, Generic, Serialize)

instance FromJSON Item where
  parseJSON = withObject "Item" $ \o -> do
    _itemId <- o .: "id"
    _itemName <- o .: "name"
    _itemDesc <- o .:? "desc" .!= ""
    _itemUse <- o .:? "use"
    pure Item {..}

instance ToJSON Item where
  toJSON Item {..} =
    object
      [ "id" .= _itemId,
        "name" .= _itemName,
        "desc" .= _itemDesc,
        "use" .= _itemUse
      ]

instance Configurable Item

makeLenses ''ItemUse
makeLenses ''Item

data ItemEntity = ItemEntity
  { _item :: Item,
    _amount :: Int
  }
  deriving (Show, Eq, Ord, Generic, Serialize)

instance FromJSON ItemEntity
instance ToJSON ItemEntity

instance Configurable ItemEntity

makeLenses ''ItemEntity

type ConcurrencyId = Text

type EffectId = Text

data Effect = Effect
  { _effectId :: EffectId,
    _effectName :: Text,
    _effectType :: EffectType,
    _effectDesc :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Effect where
  parseJSON = withObject "Effect" $ \o -> do
    _effectId <- o .: "id"
    _effectName <- o .: "name"
    _effectType <- o .: "type"
    _effectDesc <- o .: "desc"
    pure Effect {..}

instance Configurable Effect

data EffectType = DoT | HoT | Buff | DeBuff
  deriving (Show, Eq, Generic)

instance FromJSON EffectType where
  parseJSON = \case
    String "dot" -> return DoT
    String "hot" -> return HoT
    String "buff" -> return Buff
    String "debuff" -> return DeBuff
    _ -> fail "Invalid EffectType"

-- | Active effect instance applied to a character
data ActiveEffect = ActiveEffect
  { _activeEffectDef :: EffectId,
    _activeEffectRemaining :: Double,  -- Time remaining in seconds
    _activeEffectValue :: Int          -- Magnitude (damage/heal per tick, stat modifier)
  }
  deriving (Show, Eq, Generic)

makeLenses ''Effect
makeLenses ''ActiveEffect

type ActiveSkillId = Text

data ActiveSkillTarget = Single | All | Self
  deriving (Generic, Show, Eq)

instance FromJSON ActiveSkillTarget

data CombatVisualHint = CombatVisualHint
  { _combatVisualPool :: Text,
    _combatVisualAction :: Maybe Text,
    _combatVisualTags :: [Text]
  }
  deriving (Generic, Show, Eq)

makeLenses ''CombatVisualHint

instance FromJSON CombatVisualHint where
  parseJSON = withObject "CombatVisualHint" $ \o -> do
    _combatVisualPool <- o .: "pool"
    _combatVisualAction <- o .:? "action"
    _combatVisualTags <- o .:? "tags" .!= []
    pure CombatVisualHint {..}

instance ToJSON CombatVisualHint where
  toJSON CombatVisualHint {..} =
    object
      [ "pool" .= _combatVisualPool,
        "action" .= _combatVisualAction,
        "tags" .= _combatVisualTags
      ]

data ActiveSkill = ActiveSkill
  { _activeSkillId :: ActiveSkillId,
    _activeSkillName :: Text,
    _activeSkillDesc :: Text,
    _activeSkillMsg :: Text,
    _activeSkillUnlockLevel :: Int,
    _activeSkillCooldown :: Double,
    _activeSkillTarget :: ActiveSkillTarget,
    _activeSkillCost :: Int,
    _activeSkillApReq :: Int,
    _activeSkillReqArts :: [ArtId],
    _activeSkillReqStatus :: [EffectId],
    _activeSkillHeal :: Maybe Int,
    _activeSkillDamage :: Maybe Int,
    _activeSkillAnimation :: CombatVisualHint,
    _activeSkillEffSelf :: [(EffectId, Double, Int)],
    _activeSkillEffTarget :: [(EffectId, Double, Int)]
  }
  deriving (Generic, Show, Eq)

data ActiveSkillEntity = ActiveSkillEntity
  { _activeSkillDef :: ActiveSkillId,
    _activeSkillLevel :: Int
  }
  deriving (Generic, Show, Eq)

makeLenses ''ActiveSkill
makeLenses ''ActiveSkillEntity

instance FromJSON ActiveSkill where
  parseJSON = withObject "ActiveSkill" $ \o -> do
    _activeSkillId <- o .: "id"
    _activeSkillName <- o .: "name"
    _activeSkillDesc <- o .: "desc"
    _activeSkillMsg <- o .: "msg"
    _activeSkillUnlockLevel <- o .:? "unlock_level" .!= 1
    _activeSkillCooldown <- o .: "cd"
    _activeSkillTarget <- o .: "target"
    _activeSkillCost <- o .: "cost"
    _activeSkillApReq <- o .:? "ap_req" .!= 0
    _activeSkillReqArts <- o .:? "req_arts" .!= []
    _activeSkillReqStatus <- o .:? "req_status" .!= []
    _activeSkillHeal <- o .:? "heal"
    _activeSkillDamage <- o .:? "damage"
    _activeSkillAnimation <- o .: "animation"
    eff <- o .: "effect"
    _activeSkillEffSelf <- parseEffectList <$> (eff .:? "self" .!= [])
    _activeSkillEffTarget <- parseEffectList <$> (eff .:? "target" .!= [])
    pure ActiveSkill {..}
    where
      parseEffectList :: [Value] -> [(EffectId, Double, Int)]
      parseEffectList = map parseEffectItem

      parseEffectItem :: Value -> (EffectId, Double, Int)
      parseEffectItem (Object o) =
        let effId = case fromJSON <$> KM.lookup (Key.fromString "id") o of
              Just (Success eid) -> eid
              _ -> ""
            duration = case fromJSON <$> KM.lookup (Key.fromString "duration") o of
              Just (Success d) -> d
              _ -> 0.0
            value = case fromJSON <$> KM.lookup (Key.fromString "value") o of
              Just (Success v) -> v
              _ -> 0
        in (effId, duration, value)
      parseEffectItem _ = ("", 0.0, 0)

instance Configurable ActiveSkill

type AttackMoveId = Text

data AttackMove = AttackMove
  { _attackMoveId :: AttackMoveId,
    _attackMoveName :: Text,
    _attackMoveDesc :: Text,
    _attackMoveMsg :: Text,
    _attackMoveUnlockLevel :: Int,
    _attackMoveDamage :: Int,
    _attackMoveAnimation :: CombatVisualHint
  }
  deriving (Generic, Show, Eq)

makeLenses ''AttackMove

instance FromJSON AttackMove where
  parseJSON = withObject "AttackMove" $ \o -> do
    _attackMoveId <- o .: "id"
    _attackMoveName <- o .: "name"
    _attackMoveDesc <- o .: "desc"
    _attackMoveMsg <- o .: "msg"
    _attackMoveUnlockLevel <- o .:? "unlock_level" .!= 1
    _attackMoveDamage <- o .: "damage"
    _attackMoveAnimation <- o .: "animation"
    pure AttackMove {..}

data ArtType = Foundation | Internal | Lightness | Sword | Fist
  deriving (Generic, Show, Eq, Ord)

artTypeToText :: ArtType -> Text
artTypeToText = \case
  Foundation -> "foundation"
  Internal -> "internal"
  Lightness -> "lightness"
  Sword -> "sword"
  Fist -> "fist"

artTypeFromText :: Text -> Maybe ArtType
artTypeFromText = \case
  "foundation" -> Just Foundation
  "internal" -> Just Internal
  "lightness" -> Just Lightness
  "sword" -> Just Sword
  "fist" -> Just Fist
  _ -> Nothing

instance FromJSON ArtType where
  parseJSON p = case p of
    String artTypeText | Just artType' <- artTypeFromText artTypeText -> pure artType'
    _ -> fail "Invalid ArtType in MartialArt"

instance ToJSON ArtType where
  toJSON = String . artTypeToText

instance FromJSONKey ArtType where
  fromJSONKey = FromJSONKeyTextParser $ \artTypeText ->
    case artTypeFromText artTypeText of
      Just artType' -> pure artType'
      Nothing -> fail "Invalid ArtType in MartialArt"

instance ToJSONKey ArtType where
  toJSONKey = toJSONKeyText artTypeToText

data ArtRequirement = ArtRequirement
  { _artRequirementArt :: ArtId,
    _artRequirementLevel :: Int
  }
  deriving (Generic, Show, Eq)

instance FromJSON ArtRequirement where
  parseJSON = withObject "ArtRequirement" $ \o -> do
    _artRequirementArt <- o .: "art"
    _artRequirementLevel <- o .: "level"
    pure ArtRequirement {..}

data MartialArt = MartialArt
  { _artId :: ArtId,
    _artType :: ArtType,
    _artName :: Text,
    _artDesc :: Text,
    _artFoundation :: Maybe ArtId,
    _artRequires :: [ArtRequirement],
    _artMaxLevel :: Int,
    _artAttackMoves :: [AttackMove],
    _artActiveSkills :: [ActiveSkill]
  }
  deriving (Generic, Show, Eq)

data ArtEntity = ArtEntity
  { _artDef :: ArtId,
    _artLevel :: Int,
    _artProgress :: Int
  }
  deriving (Generic, Show, Eq)

instance FromJSON MartialArt where
  parseJSON = withObject "MartialArt" $ \o -> do
    _artId <- o .: "id"
    _artType <- o .: "type"
    _artName <- o .: "name"
    _artDesc <- o .: "desc"
    _artFoundation <- o .:? "foundation"
    _artRequires <- o .:? "requires" .!= []
    _artMaxLevel <- o .:? "max_level" .!= 100
    _artAttackMoves <- o .:? "attack_moves" .!= []
    _artActiveSkills <- o .:? "active_skills" .!= []
    pure MartialArt {..}

instance Configurable MartialArt

instance FromJSON ArtEntity where
  parseJSON = withObject "ArtEntity" $ \o -> do
    _artDef <- o .: "id"
    _artLevel <- o .: "level"
    _artProgress <- o .:? "progress" .!= 0
    pure ArtEntity {..}

instance ToJSON ArtEntity where
  toJSON ArtEntity {..} =
    object
      [ "id" .= _artDef,
        "level" .= _artLevel,
        "progress" .= _artProgress
      ]

makeLenses ''ArtRequirement
makeLenses ''MartialArt
makeLenses ''ArtEntity

-- | Data type representing a character in the game world.
type CharId = Text

data CharAction
  = Attacking
  | Dialogue
  | Sparring
  deriving (Show, Eq, Generic, Ord)

instance FromJSON CharAction where
  parseJSON = \case
    String "attacking" -> return Attacking
    String "sparring" -> return Sparring
    String "dialogue" -> return Dialogue
    _ -> fail "Invalid action"

data CharStatus
  = CharAlive
  | CharDead
  | CharBattle
  | CharBusy
  deriving (Show, Eq, Generic)

data Gender
  = Male
  | Female
  | UnknownGender
  deriving (Show, Eq, Ord, Generic)

genderToText :: Gender -> Text
genderToText = \case
  Male -> "male"
  Female -> "female"
  UnknownGender -> "unknown"

instance FromJSON Gender where
  parseJSON = \case
    String genderText ->
      case T.toLower genderText of
        "male" -> pure Male
        "female" -> pure Female
        "unknown" -> pure UnknownGender
        _ -> fail "Invalid Gender"
    _ -> fail "Invalid Gender"

instance ToJSON Gender where
  toJSON = String . genderToText

data InnateAttrs = InnateAttrs
  { _innateStrength :: Int,
    _innateAgility :: Int,
    _innateVitality :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON InnateAttrs where
  parseJSON = withObject "InnateAttrs" $ \o -> do
    _innateStrength <- clampInnateScore <$> o .: "strength"
    _innateAgility <- clampInnateScore <$> o .: "agility"
    _innateVitality <- clampInnateScore <$> o .: "vitality"
    pure InnateAttrs {..}

instance ToJSON InnateAttrs where
  toJSON InnateAttrs {..} =
    object
      [ "strength" .= _innateStrength,
        "agility" .= _innateAgility,
        "vitality" .= _innateVitality
      ]

data DerivedStats = DerivedStats
  { _dsMaxHp :: Int,
    _dsMaxQi :: Int,
    _dsMaxJing :: Int,
    _dsQiRegen :: Double,
    _dsJingRegen :: Double,
    _dsAttack :: Int,
    _dsDefense :: Int,
    _dsHit :: Int,
    _dsDodge :: Int,
    _dsParry :: Int,
    _dsDamageBonus :: Int,
    _dsDamageReduction :: Int,
    _dsLoadLimit :: Int
  }
  deriving (Show, Eq, Generic)

data CharacterVitals = CharacterVitals
  { _vitalsHp :: Int,
    _vitalsMaxHp :: Int,
    _vitalsQi :: Int,
    _vitalsMaxQi :: Int,
    _vitalsJing :: Int,
    _vitalsMaxJing :: Int
  }
  deriving (Show, Eq, Generic)

data CharacterProfileConfig = CharacterProfileConfig
  { profileConfigGender :: Gender,
    profileConfigAppearance :: Int
  }
  deriving (Show, Eq, Generic)

defaultAppearanceScore :: Int
defaultAppearanceScore = 5

defaultJing :: Int
defaultJing = 120

defaultInnateAttrs :: InnateAttrs
defaultInnateAttrs = InnateAttrs 18 18 18

defaultCharacterProfileConfig :: CharacterProfileConfig
defaultCharacterProfileConfig = CharacterProfileConfig UnknownGender defaultAppearanceScore

instance FromJSON CharacterProfileConfig where
  parseJSON = withObject "CharacterProfileConfig" $ \o -> do
    profileConfigGender <- o .:? "gender" .!= UnknownGender
    profileConfigAppearance <- clampAppearanceScore <$> o .:? "appearance" .!= defaultAppearanceScore
    pure CharacterProfileConfig {..}

clampInt :: Int -> Int -> Int -> Int
clampInt lower upper = max lower . min upper

clampAppearanceScore :: Int -> Int
clampAppearanceScore = clampInt 0 10

clampInnateScore :: Int -> Int
clampInnateScore = clampInt 1 60

clampLegacyInnateScore :: Int -> Int
clampLegacyInnateScore = clampInt 6 45

normalizeOldStrength :: Int -> Int
normalizeOldStrength old
  | old > 100 = 18 + min 12 ((old - 100) `div` 50)
  | otherwise = clampLegacyInnateScore old

legacyInnateAttrs :: Maybe Int -> Maybe Int -> Maybe Int -> InnateAttrs
legacyInnateAttrs legacyStrength legacyAgility legacyVitality =
  InnateAttrs
    { _innateStrength = maybe (_innateStrength defaultInnateAttrs) normalizeOldStrength legacyStrength,
      _innateAgility = maybe (_innateAgility defaultInnateAttrs) clampLegacyInnateScore legacyAgility,
      _innateVitality = maybe (_innateVitality defaultInnateAttrs) clampLegacyInnateScore legacyVitality
    }

appearanceDescription :: Int -> Text
appearanceDescription score =
  case clampAppearanceScore score of
    0 -> "面貌残破，令人不敢细看"
    1 -> "形貌粗陋，眉眼多有乖戾"
    2 -> "容色黯淡，不易令人记住"
    3 -> "相貌平常，胜在人还干净"
    4 -> "五官端正，已有几分顺眼"
    5 -> "清清爽爽，不惹眼也不失礼"
    6 -> "眉目清秀，举止间有些风致"
    7 -> "容貌出众，行在人群里很难被忽略"
    8 -> "明艳或俊逸，足以让旁人多看一眼"
    9 -> "姿容极盛，近乎江湖传闻中的人物"
    _ -> "风华照人，几可称倾城之色"

appearancePortraitKey :: Int -> Text
appearancePortraitKey score =
  let clamped = clampAppearanceScore score
      prefix = if clamped < 10 then "beauty-score-0" else "beauty-score-"
   in prefix <> pack (Prelude.show clamped)

makeLenses ''InnateAttrs
makeLenses ''DerivedStats
makeLenses ''CharacterVitals

data Character = Character
  { _charId :: CharId,
    _charName :: Text,
    _charDesc :: Text,
    _charGender :: Gender,
    _charAppearance :: Int,
    _charDialogue :: [Text],
    _charActions :: S.Set CharAction,
    _charRespawn :: Int,
    -- The character's original attributes.
    _charHP :: Int,
    _charMaxHP :: Int,
    _charQi :: Int,
    _charMaxQi :: Int,
    _charJing :: Int,
    _charQiRegen :: Double,
    _charInnate :: InnateAttrs,
    _charStrength :: Int,
    _charAgility :: Int,
    _charVitality :: Int,
    -- Equipment
    _charPrepare :: M.Map ArtType ArtEntity,
    _charEnabled :: M.Map ArtType ArtEntity,
    _charTeaches :: M.Map ArtId Int,
    _charItems :: S.Set ItemEntity,
    _charArt :: M.Map ArtType [ArtEntity],
    -- Status
    _charStatus :: CharStatus
  }
  deriving (Show, Generic, Eq)

makeLenses ''Character

newCharacter :: CharId -> Text -> Character
newCharacter cid cname =
  Character
    { _charId = cid,
      _charName = cname,
      _charDesc = "",
      _charGender = UnknownGender,
      _charAppearance = defaultAppearanceScore,
      _charDialogue = [],
      _charActions = S.empty,
      _charRespawn = 0,
      _charHP = 0,
      _charMaxHP = 0,
      _charQi = 0,
      _charMaxQi = 0,
      _charJing = defaultJing,
      _charQiRegen = 0.0,
      _charInnate = defaultInnateAttrs,
      _charStrength = 0,
      _charAgility = 0,
      _charVitality = 0,
      _charPrepare = M.empty,
      _charEnabled = M.empty,
      _charTeaches = M.empty,
      _charItems = S.empty,
      _charArt = M.empty,
      _charStatus = CharAlive
    }

instance FromJSON Character where
  parseJSON = withObject "Character" $ \o -> do
    _charId <- o .: "id"
    _charName <- o .: "name"
    _charDesc <- o .: "desc"
    profile <- o .:? "profile" .!= defaultCharacterProfileConfig
    rootGender <- o .:? "gender"
    rootAppearance <- o .:? "appearance"
    let _charGender = fromMaybe (profileConfigGender profile) rootGender
    let _charAppearance = clampAppearanceScore $ fromMaybe (profileConfigAppearance profile) rootAppearance
    _charDialogue <- o .: "dialogue"
    _charActions <- o .: "actions"
    _charRespawn <- o .: "respawn"

    attr <- o .: "attr"
    _charHP <- attr .: "hp"
    _charMaxHP <- attr .:? "max_hp" .!= _charHP
    _charQi <- attr .:? "qi" .!= 0
    _charMaxQi <- attr .:? "max_qi" .!= 0
    _charJing <- attr .:? "jing" .!= defaultJing
    _charQiRegen <- attr .:? "qi_regen" .!= 0.0
    legacyStrength <- attr .:? "str"
    legacyAgility <- attr .:? "agi"
    legacyVitality <- attr .:? "vit"
    innateMaybe <- attr .:? "innate"
    let _charInnate = fromMaybe (legacyInnateAttrs legacyStrength legacyAgility legacyVitality) innateMaybe
    let _charStrength = fromMaybe (_innateStrength _charInnate) legacyStrength
    let _charAgility = fromMaybe (_innateAgility _charInnate) legacyAgility
    let _charVitality = fromMaybe (_innateVitality _charInnate) legacyVitality

    _charArt <- o .: "martial_arts"
    _charPrepare <- o .: "prepared"
    _charEnabled <- o .:? "enabled" .!= _charPrepare
    _charTeaches <- o .:? "teaches" .!= M.empty
    -- _charItems <- o .: "items"
    -- _charRoutine <- o .: "routine"
    let _charItems = S.empty
    -- let _charArt = M.empty
    let _charStatus = CharAlive
    return Character {..}

instance Configurable Character

type PlayerId = Text

data PlayerStatus = PlayerNormal | PlayerInBattle | PlayerDead | PlayerBanned
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PlayerStatus where
  parseJSON = \case
    String "normal" -> return PlayerNormal
    String "inbattle" -> return PlayerInBattle
    String "dead" -> return PlayerDead
    String "banned" -> return PlayerBanned
    _ -> fail "Invalid PlayerStatus"

instance FromJSONKey PlayerStatus where
  fromJSONKey = FromJSONKeyTextParser $ \case
    "normal" -> pure PlayerNormal
    "inbattle" -> pure PlayerInBattle
    "dead" -> pure PlayerDead
    "banned" -> pure PlayerBanned
    _ -> fail "Invalid PlayerStatus"

instance ToJSON PlayerStatus where
  toJSON = \case
    PlayerNormal -> String "normal"
    PlayerInBattle -> String "inbattle"
    PlayerDead -> String "dead"
    PlayerBanned -> String "banned"

data Player = Player
  { _playerId :: PlayerId,
    _playerPosition :: (Text, (Int, Int)),
    _playerConcurrency :: M.Map ConcurrencyId Int,
    _playerStatus :: PlayerStatus,
    _playerInventory :: M.Map ItemId Int,
    _playerMoney :: Int,
    _playerPotential :: Int,
    _playerCombatExp :: Int,
    _playerCharacter :: Character
  }
  deriving (Show, Eq, Generic)

makeLenses ''Player

instance FromJSON Player where
  parseJSON = withObject "Player" $ \o -> do
    _playerId <- o .: "id"
    _playerPosition <- o .: "position"
    _playerConcurrency <- o .: "concurrency"
    _playerStatus <- o .: "status"
    _playerInventory <- o .:? "inventory" .!= M.empty
    _playerMoney <- o .:? "money" .!= 0
    _playerPotential <- o .:? "potential" .!= 0
    _playerCombatExp <- o .:? "combat_exp" .!= 0
    _playerCharacter <- o .: "char"
    return Player {..}

instance Configurable Player

newPlayer :: PlayerId -> Text -> Player
newPlayer pid cname =
  Player
    { _playerId = pid,
      _playerPosition = ("", (0, 0)),
      _playerConcurrency = M.empty,
      _playerStatus = PlayerNormal,
      _playerInventory = M.empty,
      _playerMoney = 0,
      _playerPotential = 0,
      _playerCombatExp = 0,
      _playerCharacter = newCharacter cid cname
    }
  where
    cid = "player$char$" <> pid

deriveStats :: Player -> DerivedStats
deriveStats player =
  DerivedStats
    { _dsMaxHp = 80 + vit * 5 + internalPower * 4 + combatExpHpBonus,
      _dsMaxQi = char ^. charMaxQi + vit * 2 + internalPower * 6,
      _dsMaxJing = 80 + vit * 4 + internalPower * 2 + combatExpJingBonus,
      _dsQiRegen = 1.0 + fromIntegral vit / 25.0 + fromIntegral internalPower / 30.0,
      _dsJingRegen = 0.5 + fromIntegral vit / 40.0 + fromIntegral internalPower / 60.0,
      _dsAttack = 30 + meleeLevel * 7 + foundationMelee * 2 + str * 3 + agi * 2,
      _dsDefense = damageReduction,
      _dsHit = str * 2 + agi * 2 + meleeLevel * 3,
      _dsDodge = 20 + agi * 5 + vit * 2 + lightnessPower * 6,
      _dsParry = 15 + vit * 4 + str * 2 + parryArtLevel * 6,
      _dsDamageBonus = max 0 ((str - 10) `div` 3),
      _dsDamageReduction = damageReduction,
      _dsLoadLimit = 20000 + str * 1500 + basicFist * 100
    }
  where
    char = player ^. playerCharacter
    innate = char ^. charInnate
    str = clampEffectiveAttr $ innate ^. innateStrength + basicFist `div` 20
    agi = clampEffectiveAttr $ innate ^. innateAgility + basicLightness `div` 20
    vit = clampEffectiveAttr $ innate ^. innateVitality + basicInternal `div` 20
    basicInternal = characterKnownArtLevel "basic_internal" char
    basicLightness = characterKnownArtLevel "basic_lightness" char
    basicFist = characterKnownArtLevel "basic_fist" char
    basicSword = characterKnownArtLevel "basic_sword" char
    enabledInternal = equippedArtLevel Internal char
    enabledLightness = equippedArtLevel Lightness char
    internalPower = basicInternal `div` 2 + enabledInternal
    lightnessPower = basicLightness `div` 2 + enabledLightness
    meleeLevel = max (equippedArtLevel Sword char) (equippedArtLevel Fist char)
    foundationMelee = max basicFist basicSword
    parryArtLevel = meleeLevel
    combatExpHpBonus = min 80 (player ^. playerCombatExp `div` 2000)
    combatExpJingBonus = min 60 (player ^. playerCombatExp `div` 3000)
    damageReduction = max 0 ((vit - 10) `div` 4) + internalPower `div` 12

clampEffectiveAttr :: Int -> Int
clampEffectiveAttr = clampInt 1 60

characterKnownArtLevel :: ArtId -> Character -> Int
characterKnownArtLevel targetArtId char =
  case artLevels of
    [] -> 0
    _ -> maximum artLevels
  where
    artLevels =
      [ known ^. artLevel
        | knownArts <- M.elems $ char ^. charArt,
          known <- knownArts,
          known ^. artDef == targetArtId
      ]

equippedArtLevel :: ArtType -> Character -> Int
equippedArtLevel artType' char =
  maybe 0 (^. artLevel) $ M.lookup artType' (char ^. charEnabled)

data Direction
  = North
  | South
  | East
  | West
  | NorthEast
  | NorthWest
  | SouthEast
  | SouthWest
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Direction

instance ToJSON Direction

instance FromJSONKey Direction where
  fromJSONKey = FromJSONKeyTextParser $ \case
    "north" -> pure North
    "south" -> pure South
    "east" -> pure East
    "west" -> pure West
    "northeast" -> pure NorthEast
    "northwest" -> pure NorthWest
    "southeast" -> pure SouthEast
    "southwest" -> pure SouthWest
    _ -> fail "Invalid direction"

instance ToJSONKey Direction where
  toJSONKey = toJSONKeyText $ \case
    North -> "north"
    South -> "south"
    East -> "east"
    West -> "west"
    NorthEast -> "northeast"
    NorthWest -> "northwest"
    SouthEast -> "southeast"
    SouthWest -> "southwest"

type MapId = Text

type RoomId = Text

data RoomRef = RoomRef
  { _roomRefMapId :: MapId,
    _roomRefPos :: (Int, Int)
  }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''RoomRef

instance ToJSON RoomRef where
  toJSON RoomRef {..} =
    object
      [ "map" .= _roomRefMapId,
        "position" .= _roomRefPos
      ]

data RoomExitConfig
  = LocalRoomExit (Int, Int)
  | CrossMapRoomExit RoomRef
  deriving (Show, Eq, Generic)

instance FromJSON RoomExitConfig where
  parseJSON value =
    parseCrossMapExit value <|> parseLocalExit value
    where
      parseCrossMapExit = withObject "RoomExit" $ \o ->
        CrossMapRoomExit <$> (RoomRef <$> o .: "map" <*> o .: "position")

      parseLocalExit = fmap LocalRoomExit . parseJSON

roomExitConfigToRef :: MapId -> RoomExitConfig -> RoomRef
roomExitConfigToRef currentMapId = \case
  LocalRoomExit pos -> RoomRef currentMapId pos
  CrossMapRoomExit roomRef -> roomRef

data Room = Room
  { _roomId :: RoomId,
    _roomPos :: (Int, Int),
    _roomName :: Text,
    _roomDesc :: Text,
    _roomScript :: Maybe Text, -- Change this to store the Lua script content
    _roomExits :: M.Map Direction RoomRef,
    _roomChar :: [CharId],
    _roomPlayer :: S.Set PlayerId
  }
  deriving (Show, Eq, Generic)

makeLenses ''Room

instance FromJSON Room where
  parseJSON = withObject "Room" $ \o -> do
    _roomId <- o .: "id" 
    _roomPos <- o .: "position"
    _roomName <- o .: "name"
    _roomDesc <- o .: "desc"
    _roomScript <- o .:? "script"
    exitConfigs <- o .: "exits"
    let _roomExits = M.map (roomExitConfigToRef "") exitConfigs
    _roomChar <- o .:? "char" .!= []
    let _roomPlayer = S.empty
    return Room {..}

instance ToJSON Room where
  toJSON Room {..} =
    object
      [ "id" .= _roomId,
        "name" .= _roomName,
        "desc" .= _roomDesc,
        "script" .= _roomScript,
        "exits" .= _roomExits,
        "char" .= _roomChar
      ]

data Map = Map
  { _mapId :: Text,
    _mapName :: Text,
    _mapDesc :: Text,
    _mapRooms :: M.Map (Int, Int) Room
  }
  deriving (Show, Eq, Generic)

makeLenses ''Map

instance FromJSON Map where
  parseJSON = withObject "Character" $ \o -> do
    _mapId <- o .: "id"
    _mapName <- o .: "name"
    _mapDesc <- o .: "desc"
    -- _mapRooms <- o .: "rooms"
    rs <- o .: "rooms"
    let rsWithResolvedExits = map (roomExits %~ M.map (fillLocalExitMap _mapId)) rs
        fillLocalExitMap targetMapId roomRef =
          if T.null (roomRef ^. roomRefMapId)
            then roomRef & roomRefMapId .~ targetMapId
            else roomRef
        _mapRooms = M.fromList $ map (\r -> (r ^. roomPos, r)) rsWithResolvedExits
    return Map {..}

instance Configurable Map
