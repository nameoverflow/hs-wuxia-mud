{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.World where

import Control.Lens
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Text as T
import Game.Entity
import Game.Quest
import Relude hiding (Map, show)
import System.FilePath ((</>))
import Utils

-- world properties and configures
data World = World
  { _items :: M.Map ItemId Item,
    _maps :: M.Map MapId Map,
    _chars :: M.Map CharId Character,
    _quests :: M.Map T.Text Quest,
    _skills :: M.Map MaId MartialArt
  }
  deriving (Eq, Show, Generic)

makeLenses ''World

data WorldException
  = MapNotFound MapId
  | RoomNotFound MapId (Int, Int)
  | ItemNotFound ItemId
  | SkillNotFound MaId
  | QuestNotFound T.Text
  | CharNotFound CharId
  deriving (Eq, Show, Generic)

type WorldStateT = StateT World (ExceptT WorldException IO)

loadAllAssets :: FilePath -> IO (Either T.Text World)
loadAllAssets basePath = do
  -- itemResult <- loadItems $ basePath </> "items"
  -- skillResult <- loadSkills $ basePath </> "skills"
  -- npcResult <- loadNPCs $ basePath </> "npcs"
  -- questResult <- loadQuests $ basePath </> "quests"
  mapResult <- loadMaps $ basePath </> "maps"

  -- return $ case (itemResult, skillResult, npcResult, questResult, mapResult) of
  return $ case mapResult of
    -- (Right items, Right skills, Right npcs, Right quests, Right maps) ->
    Right maps ->
      Right
        World
          { _items = M.empty,
            _maps = maps,
            _chars = M.empty,
            _quests = M.empty,
            _skills = M.empty
          }
    _ ->
      Left $
        T.unlines
          -- [ fromMaybe "" $ leftToMaybe itemResult,
          --   fromMaybe "" $ leftToMaybe skillResult,
          --   fromMaybe "" $ leftToMaybe npcResult,
          --   fromMaybe "" $ leftToMaybe questResult,
          [ fromMaybe "" $ leftToMaybe mapResult
          ]

getsMap :: T.Text -> WorldStateT Map
getsMap mId = getsL maps mId $ MapNotFound mId

getsMapRoom :: T.Text -> (Int, Int) -> WorldStateT Room
getsMapRoom mId pos = do
  m <- getsMap mId
  case M.lookup pos $ m ^. mapRooms of
    Nothing -> throwError $ RoomNotFound mId pos
    Just room -> return room

getsCharacter :: CharId -> WorldStateT Character
getsCharacter cId = getsL chars cId $ CharNotFound cId

getsMartialArt :: MaId -> WorldStateT MartialArt
getsMartialArt rId = getsL skills rId $ SkillNotFound rId
