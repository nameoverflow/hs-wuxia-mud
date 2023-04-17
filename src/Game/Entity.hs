{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Entity where

import Control.Applicative ((<|>))
import Control.Lens (makeLenses, (^.))
import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON, ToJSONKey (..), Value (..), withObject, (.:), (.:?))
-- import Data.Yaml (FromJSON, ParseException, decodeEither', decodeFileEither, withObject, (.:), (.:?))
-- import Data.Yaml.Aeson (FromJSON (..), Value (..))
-- import Data.Yaml.Parser (typeMismatch)

import Data.Aeson.Types
import qualified Data.Map as M
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import qualified Data.Set as S
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO
import GHC.Generics (Generic)
import Relude (toText)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Utils

type ItemId = Text

data Item = Item
  { _itemId :: ItemId,
    _itemName :: Text
  }
  deriving (Show, Eq, Generic, Serialize)

instance FromJSON Item

makeLenses ''Item

data ItemEntity = ItemEntity
  { _item :: Item,
    _amount :: Int
  }
  deriving (Show, Eq, Generic, Serialize)

instance FromJSON ItemEntity

instance Configurable ItemEntity

makeLenses ''ItemEntity

type ConcurrencyId = Text

type EffectId = Text

data Effect = Effect
  { effectId :: EffectId,
    effectName :: Text,
    effectType :: EffectType
  }
  deriving (Show, Eq)

data EffectType = DoT | HoT | Buff | DeBuff
  deriving (Show, Eq)

-- | Skills are active skills that can be used in battle, bind to a tech
type SkillId = Text

data SkillTarget = Single | All | Self
  deriving (Generic, Show, Eq)

instance FromJSON SkillTarget

data Skill = Skill
  { _skillId :: SkillId,
    _skillName :: Text,
    _skillDesc :: Text,
    _skillMsg :: Text,
    _skillCooldown :: Double,
    _skillTarget :: SkillTarget,
    _skillCost :: Int,
    _skillHeal :: Maybe Int,
    _skillDamage :: Maybe Int,
    _skillEffSelf :: [EffectId],
    _skillEffTarget :: [EffectId]
  }
  deriving (Generic, Show, Eq)

data SkillEntity = SkillEntity
  { _skillDef :: SkillId,
    _skillLevel :: Int
  }
  deriving (Generic, Show, Eq)

makeLenses ''Skill
makeLenses ''SkillEntity

instance FromJSON Skill where
  parseJSON = withObject "Skill" $ \o -> do
    _skillId <- o .: "id"
    _skillName <- o .: "name"
    _skillDesc <- o .: "desc"
    _skillMsg <- o .: "msg"
    _skillCooldown <- o .: "cd"
    _skillTarget <- o .: "target"
    _skillCost <- o .: "cost"
    _skillHeal <- o .:? "heal"
    _skillDamage <- o .:? "damage"
    eff <- o .: "effect"
    _skillEffSelf <- eff .:? "self" .!= []
    _skillEffTarget <- eff .:? "target" .!= []
    pure Skill {..}

instance Configurable Skill

-- | Moves are normal attacks that bind to a tech
data Move = Move
  { _moveId :: SkillId,
    _moveName :: Text,
    _moveDesc :: Text,
    _moveMsg :: Text,
    _moveDamage :: Int
  }
  deriving (Generic, Show, Eq)

makeLenses ''Move

instance FromJSON Move where
  parseJSON = withObject "Move" $ \o -> do
    _moveId <- o .: "id"
    _moveName <- o .: "name"
    _moveDesc <- o .: "desc"
    _moveMsg <- o .: "msg"
    _moveDamage <- o .: "damage"
    pure Move {..}

-- | Routines are sets of skills and moves, can be learned by character
type ArtId = Text

data ArtType = Technique | Cultivation | Lightness
  deriving (Generic, Show, Eq, Ord)

instance FromJSON ArtType where
  parseJSON p = case p of
    String "technique" -> pure Technique
    String "cultivation" -> pure Cultivation
    String "lightness" -> pure Lightness
    _ -> fail "Invalid ArtType in MartialArt"

instance FromJSONKey ArtType where
  fromJSONKey = FromJSONKeyTextParser $ \case
    "technique" -> pure Technique
    "cultivation" -> pure Cultivation
    "lightness" -> pure Lightness
    _ -> fail "Invalid ArtType in MartialArt"

data MartialArt = MartialArt
  { _artId :: ArtId,
    _artType :: ArtType,
    _artName :: Text,
    _artDesc :: Text,
    _artMoves :: [Move],
    _artSkills :: [Skill]
  }
  deriving (Generic, Show, Eq)

data ArtEntity = ArtEntity
  { _artDef :: ArtId,
    _artLevel :: Int
  }
  deriving (Generic, Show, Eq)

instance FromJSON MartialArt where
  parseJSON = withObject "MartialArt" $ \o -> do
    _artId <- o .: "id"
    _artType <- o .: "type"
    _artName <- o .: "name"
    _artDesc <- o .: "desc"
    _artMoves <- o .:? "moves" .!= []
    _artSkills <- o .:? "skills" .!= []
    pure MartialArt {..}

instance Configurable MartialArt

instance FromJSON ArtEntity where
  parseJSON = withObject "ArtEntity" $ \o -> do
    _artDef <- o .: "id"
    _artLevel <- o .: "level"
    pure ArtEntity {..}

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

data Character = Character
  { _charId :: CharId,
    _charName :: Text,
    _charDesc :: Text,
    _charDialogue :: [Text],
    _charActions :: S.Set CharAction,
    _charRespawn :: Int,
    -- The character's original attributes.
    _charHP :: Int,
    _charStrength :: Int,
    _charAgility :: Int,
    _charVitality :: Int,
    -- Equipment
    _charPrepare :: M.Map ArtType ArtEntity,
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
      _charDialogue = [],
      _charActions = S.empty,
      _charRespawn = 0,
      _charHP = 0,
      _charStrength = 0,
      _charAgility = 0,
      _charVitality = 0,
      _charPrepare = M.empty,
      _charItems = S.empty,
      _charArt = M.empty,
      _charStatus = CharAlive
    }

instance FromJSON Character where
  parseJSON = withObject "Character" $ \o -> do
    _charId <- o .: "id"
    _charName <- o .: "name"
    _charDesc <- o .: "desc"
    _charDialogue <- o .: "dialogue"
    _charActions <- o .: "actions"
    _charRespawn <- o .: "respawn"

    attr <- o .: "attr"
    _charHP <- attr .: "hp"
    _charStrength <- attr .: "str"
    _charAgility <- attr .: "agi"
    _charVitality <- attr .: "vit"

    _charArt <- o .: "skills"
    _charPrepare <- o .: "prepared"
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

data Player = Player
  { _playerId :: PlayerId,
    _playerPosition :: (Text, (Int, Int)),
    _playerConcurrency :: M.Map ConcurrencyId Int,
    _playerStatus :: PlayerStatus,
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
      _playerCharacter = newCharacter cid cname
    }
  where
    cid = "player$char$" <> pid

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

type RoomId = Text

data Room = Room
  { _roomId :: RoomId,
    _roomPos :: (Int, Int),
    _roomName :: Text,
    _roomDesc :: Text,
    _roomScript :: Maybe Text, -- Change this to store the Lua script content
    _roomExits :: M.Map Direction (Int, Int),
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
    _roomExits <- o .: "exits"
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

type MapId = Text

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
    rs <- o.: "rooms"
    let _mapRooms = M.fromList $ map (\r -> (r ^. roomPos, r)) rs
    return Map {..}

instance Configurable Map