{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Entity where

import Control.Applicative ((<|>))
import Control.Lens (makeLenses)
import Data.Aeson (FromJSONKey, FromJSONKeyFunction (..))
import Data.Aeson.Types (FromJSONKey (..))
import qualified Data.Map as M
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import qualified Data.Set as S
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO
import Data.Yaml (FromJSON, ParseException, decodeEither', decodeFileEither, withObject, (.:))
import Data.Yaml.Aeson (FromJSON (..), Value (..))
import Data.Yaml.Parser (typeMismatch)
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

makeLenses ''ItemEntity

type ConcurrencyId = Text

-- Load yaml definitions of items from a directory
loadItems :: FilePath -> IO (Either Text (M.Map ItemId Item))
loadItems = loadYamlProperties $ \it -> (_itemId it, it)

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

instance FromJSON Skill

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

instance FromJSON Move

-- | Routines are sets of skills and moves, can be learned by character
type MaId = Text

data MaType = Technique | Cultivation | Lightness
  deriving (Generic, Show, Eq, Ord)

instance FromJSON MaType where
  parseJSON p = case p of
    String "technique" -> pure Technique
    String "cultivation" -> pure Cultivation
    String "lightness" -> pure Lightness
    _ -> fail "Invalid MaType in MartialArt"

instance FromJSONKey MaType where
  fromJSONKey = FromJSONKeyTextParser $ \case
    "technique" -> pure Technique
    "cultivation" -> pure Cultivation
    "lightness" -> pure Lightness
    _ -> fail "Invalid MaType in MartialArt"

data MartialArt = MartialArt
  { _techId :: MaId,
    _techType :: MaType,
    _techName :: Text,
    _techDesc :: Text,
    _techMoves :: [Move],
    _techSkills :: [Skill]
  }
  deriving (Generic, Show, Eq)

data MaEntity = MaEntity
  { _techDef :: MaId,
    _techLevel :: Int
  }
  deriving (Generic, Show, Eq)

instance FromJSON MartialArt

instance FromJSON MaEntity

makeLenses ''MartialArt
makeLenses ''MaEntity

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
    _charEqMa :: M.Map MaType MaEntity,
    _charItems :: S.Set ItemEntity,
    _charMa :: M.Map MaType [MaEntity],
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
      _charEqMa = M.empty,
      _charItems = S.empty,
      _charMa = M.empty,
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
    _charStrength <- attr .: "strength"
    _charAgility <- attr .: "agility"
    _charVitality <- attr .: "vitality"

    sk <- o .: "skills"
    _charEqMa <- sk .: "equipped_martial_arts"
    -- _charItems <- o .: "items"
    -- _charRoutine <- o .: "routine"
    let _charItems = S.empty
    let _charMa = M.empty
    let _charStatus = CharAlive
    return Character {..}

-- | Load a list of characters from a YAML file.
loadCharacters :: FilePath -> IO [Character]
loadCharacters path = do
  chars <- decodeFileEither path :: IO (Either ParseException [Character])
  case chars of
    Left err -> error $ "Error loading character file: " ++ show err
    Right cs -> return cs

type PlayerId = Text

data PlayerStatus = PlayerNormal | PlayerInBattle | PlayerDead | PlayerBanned
  deriving (Show, Eq, Ord, Generic)

data Player = Player
  { _playerId :: PlayerId,
    _playerPosition :: (Text, (Int, Int)),
    _playerConcurrency :: M.Map ConcurrencyId Int,
    _playerStatus :: PlayerStatus,
    _playerCharacter :: Character
  }
  deriving (Show, Eq, Generic)

makeLenses ''Player

newPlayer :: PlayerId -> Text -> Player
newPlayer pid cname = Player
  { _playerId = pid,
    _playerPosition = ("", (0, 0)),
    _playerConcurrency = M.empty,
    _playerStatus = PlayerNormal,
    _playerCharacter = newCharacter cid cname
  }
  where cid = "player$char$" <> pid

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

instance FromJSONKey Direction

type RoomId = Text

data Room = Room
  { _roomId :: RoomId,
    _roomName :: Text,
    _roomDesc :: Text,
    _roomScript :: Maybe Text, -- Change this to store the Lua script content
    _roomExits :: M.Map Direction (Int, Int),
    _roomChar :: [CharId],
    _roomPlayer :: S.Set PlayerId
  }
  deriving (Show, Eq, Generic)

makeLenses ''Room

instance FromJSON Room

type MapId = Text

data Map = Map
  { _mapId :: Text,
    _mapRooms :: M.Map (Int, Int) Room
  }
  deriving (Show, Eq, Generic)

makeLenses ''Map

instance FromJSON Map

loadMap :: FilePath -> IO (Either Text Map)
loadMap path = do
  fileContent <- Data.Text.IO.readFile path
  case decodeEither' (encodeUtf8 fileContent) of
    Left err -> return . Left $ pack ("Error loading map: " <> show err)
    Right (mapData :: Map) -> do
      -- Load Lua scripts for each room
      roomsWithScripts <- mapM loadRoomLuaScript $ _mapRooms mapData
      return $ Right $ mapData {_mapRooms = roomsWithScripts}

loadRoomLuaScript :: Room -> IO Room
loadRoomLuaScript room = case _roomScript room of
  Just scriptPath -> do
    scriptContent <- Data.Text.IO.readFile $ show scriptPath
    return $ room {_roomScript = Just scriptContent}
  Nothing -> return room

loadMaps :: FilePath -> IO (Either Text (M.Map Text Map))
loadMaps path = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      files <- listDirectory path
      let mapFiles = Prelude.filter (\f -> takeExtension f == ".yaml") files
      mapData <- mapM loadMap mapFiles
      let mapEither = sequence mapData
      return $ M.fromList . Prelude.map (\m -> (_mapId m, m)) <$> mapEither
    else return $ Left "Path is not a directory"