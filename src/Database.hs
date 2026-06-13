{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database
  ( PlayerSave (..),
    loadPlayerSave,
    savePlayerSave,
    savePlayerState,
    saveAllPlayerSaves,
    deletePlayerSave,
    applyPlayerSaveToGameState,
  )
where

import Control.Lens hiding ((.=))
import Control.Monad (forM_, when)
import Data.Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Game.Entity
import Game.Quest
import GameState
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))

data PlayerSave = PlayerSave
  { savePlayerId :: PlayerId,
    saveStory :: PlayerStoryState,
    saveInventory :: M.Map ItemId Int,
    saveMoney :: Int,
    saveArts :: M.Map ArtType [ArtEntity],
    savePrepared :: M.Map ArtType ArtEntity
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlayerSave where
  parseJSON = withObject "PlayerSave" $ \o -> do
    savePlayerId <- o .: "player_id"
    saveStory <- o .:? "story" .!= newPlayerStoryState
    saveInventory <- o .:? "inventory" .!= M.empty
    saveMoney <- o .:? "money" .!= 0
    saveArts <- o .:? "arts" .!= M.empty
    savePrepared <- o .:? "prepared" .!= M.empty
    pure PlayerSave {..}

instance ToJSON PlayerSave where
  toJSON PlayerSave {..} =
    object
      [ "player_id" .= savePlayerId,
        "story" .= saveStory,
        "inventory" .= saveInventory,
        "money" .= saveMoney,
        "arts" .= saveArts,
        "prepared" .= savePrepared
      ]

loadPlayerSave :: FilePath -> PlayerId -> IO (Either Text (Maybe PlayerSave))
loadPlayerSave saveDir pid = do
  let path = playerSavePath saveDir pid
  exists <- doesFileExist path
  if exists
    then fmap Just . first T.pack <$> eitherDecodeFileStrict' path
    else pure $ Right Nothing

savePlayerSave :: FilePath -> PlayerSave -> IO ()
savePlayerSave saveDir save = do
  createDirectoryIfMissing True saveDir
  BL.writeFile (playerSavePath saveDir $ savePlayerId save) (encode save)

savePlayerState :: FilePath -> PlayerId -> GameState -> IO ()
savePlayerState saveDir pid gs =
  case playerSaveFromGameState pid gs of
    Nothing -> pure ()
    Just save -> savePlayerSave saveDir save

saveAllPlayerSaves :: FilePath -> GameState -> IO ()
saveAllPlayerSaves saveDir gs =
  forM_ (M.keys $ gs ^. players) $ \pid ->
    savePlayerState saveDir pid gs

deletePlayerSave :: FilePath -> PlayerId -> IO ()
deletePlayerSave saveDir pid = do
  let path = playerSavePath saveDir pid
  exists <- doesFileExist path
  when exists $ removeFile path

applyPlayerSaveToGameState :: PlayerSave -> GameState -> GameState
applyPlayerSaveToGameState PlayerSave {..} =
  (players . ix savePlayerId . playerInventory .~ saveInventory)
    . (players . ix savePlayerId . playerMoney .~ saveMoney)
    . (players . ix savePlayerId . playerCharacter . charArt .~ saveArts)
    . (players . ix savePlayerId . playerCharacter . charPrepare .~ savePrepared)
    . (stories . at savePlayerId ?~ saveStory)

playerSaveFromGameState :: PlayerId -> GameState -> Maybe PlayerSave
playerSaveFromGameState pid gs = do
  player <- M.lookup pid (gs ^. players)
  let savePlayerId = pid
      saveStory = fromMaybe newPlayerStoryState $ M.lookup pid (gs ^. stories)
      saveInventory = player ^. playerInventory
      saveMoney = player ^. playerMoney
      saveArts = player ^. playerCharacter . charArt
      savePrepared = player ^. playerCharacter . charPrepare
  pure PlayerSave {..}

playerSavePath :: FilePath -> PlayerId -> FilePath
playerSavePath saveDir pid = saveDir </> T.unpack (sanitizePlayerId pid) <> ".json"

sanitizePlayerId :: PlayerId -> Text
sanitizePlayerId =
  T.map $ \ch ->
    if isAlphaNum ch || ch == '-' || ch == '_' || ch == '.'
      then ch
      else '_'
