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
import Control.Applicative ((<|>))
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
  { saveVersion :: Int,
    savePlayerId :: PlayerId,
    saveStory :: PlayerStoryState,
    saveInventory :: M.Map ItemId Int,
    saveMoney :: Int,
    savePotential :: Int,
    saveCombatExp :: Int,
    saveDesc :: Maybe Text,
    saveHp :: Maybe Int,
    saveMaxHp :: Maybe Int,
    saveQi :: Maybe Int,
    saveMaxQi :: Maybe Int,
    saveJing :: Maybe Int,
    saveGender :: Maybe Gender,
    saveAppearance :: Maybe Int,
    saveInnate :: Maybe InnateAttrs,
    saveArts :: M.Map ArtType [ArtEntity],
    savePrepared :: M.Map ArtType ArtEntity,
    saveEnabled :: M.Map ArtType ArtEntity
  }
  deriving (Show, Eq, Generic)

data SaveProfile = SaveProfile
  { saveProfileGender :: Maybe Gender,
    saveProfileAppearance :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data SaveCharacter = SaveCharacter
  { saveCharacterDesc :: Maybe Text,
    saveCharacterHp :: Maybe Int,
    saveCharacterMaxHp :: Maybe Int,
    saveCharacterQi :: Maybe Int,
    saveCharacterMaxQi :: Maybe Int,
    saveCharacterJing :: Maybe Int,
    saveCharacterInnate :: Maybe InnateAttrs
  }
  deriving (Show, Eq, Generic)

emptySaveProfile :: SaveProfile
emptySaveProfile = SaveProfile Nothing Nothing

emptySaveCharacter :: SaveCharacter
emptySaveCharacter = SaveCharacter Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance FromJSON SaveProfile where
  parseJSON = withObject "SaveProfile" $ \o -> do
    saveProfileGender <- o .:? "gender"
    rawAppearance <- o .:? "appearance"
    let saveProfileAppearance = clampAppearanceScore <$> rawAppearance
    pure SaveProfile {..}

instance FromJSON SaveCharacter where
  parseJSON = withObject "SaveCharacter" $ \o -> do
    saveCharacterDesc <- o .:? "desc"
    saveCharacterHp <- o .:? "hp"
    saveCharacterMaxHp <- o .:? "max_hp"
    saveCharacterQi <- o .:? "qi"
    saveCharacterMaxQi <- o .:? "max_qi"
    saveCharacterJing <- o .:? "jing"
    saveCharacterInnate <- o .:? "innate"
    pure SaveCharacter {..}

instance FromJSON PlayerSave where
  parseJSON = withObject "PlayerSave" $ \o -> do
    saveVersion <- o .:? "version" .!= 1
    savePlayerId <- o .: "player_id"
    profile <- o .:? "profile" .!= emptySaveProfile
    character <- o .:? "character" .!= emptySaveCharacter
    saveStory <- o .:? "story" .!= newPlayerStoryState
    saveInventory <- o .:? "inventory" .!= M.empty
    saveMoney <- o .:? "money" .!= 0
    savePotential <- o .:? "potential" .!= 0
    saveCombatExp <- o .:? "combat_exp" .!= 0
    topHp <- o .:? "hp"
    topMaxHp <- o .:? "max_hp"
    topQi <- o .:? "qi"
    topMaxQi <- o .:? "max_qi"
    topJing <- o .:? "jing"
    topDesc <- o .:? "desc"
    topGender <- o .:? "gender"
    topAppearance <- o .:? "appearance"
    topInnate <- o .:? "innate"
    let saveHp = topHp <|> saveCharacterHp character
    let saveMaxHp = topMaxHp <|> saveCharacterMaxHp character
    let saveQi = topQi <|> saveCharacterQi character
    let saveMaxQi = topMaxQi <|> saveCharacterMaxQi character
    let saveJing = topJing <|> saveCharacterJing character
    let saveDesc = topDesc <|> saveCharacterDesc character
    let saveGender = topGender <|> saveProfileGender profile
    let saveAppearance = (clampAppearanceScore <$> topAppearance) <|> saveProfileAppearance profile
    let saveInnate = topInnate <|> saveCharacterInnate character
    saveArts <- o .:? "arts" .!= M.empty
    savePrepared <- o .:? "prepared" .!= M.empty
    saveEnabled <- o .:? "enabled" .!= savePrepared
    pure PlayerSave {..}

instance ToJSON PlayerSave where
  toJSON PlayerSave {..} =
    object
      [ "version" .= saveVersion,
        "player_id" .= savePlayerId,
        "story" .= saveStory,
        "inventory" .= saveInventory,
        "money" .= saveMoney,
        "potential" .= savePotential,
        "combat_exp" .= saveCombatExp,
        "profile"
          .= object
            [ "gender" .= saveGender,
              "appearance" .= saveAppearance
            ],
        "character"
          .= object
            [ "desc" .= saveDesc,
              "innate" .= saveInnate,
              "hp" .= saveHp,
              "max_hp" .= saveMaxHp,
              "qi" .= saveQi,
              "max_qi" .= saveMaxQi,
              "jing" .= saveJing
            ],
        "hp" .= saveHp,
        "max_hp" .= saveMaxHp,
        "qi" .= saveQi,
        "max_qi" .= saveMaxQi,
        "jing" .= saveJing,
        "desc" .= saveDesc,
        "innate" .= saveInnate,
        "arts" .= saveArts,
        "prepared" .= savePrepared,
        "enabled" .= saveEnabled
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
    . (players . ix savePlayerId . playerPotential .~ savePotential)
    . (players . ix savePlayerId . playerCombatExp .~ saveCombatExp)
    . (players . ix savePlayerId . playerCharacter . charArt .~ saveArts)
    . (players . ix savePlayerId . playerCharacter . charPrepare .~ savePrepared)
    . (players . ix savePlayerId . playerCharacter . charEnabled .~ saveEnabled)
    . maybe id (\hp -> players . ix savePlayerId . playerCharacter . charHP .~ hp) saveHp
    . maybe id (\maxHp -> players . ix savePlayerId . playerCharacter . charMaxHP .~ maxHp) saveMaxHp
    . maybe id (\qi -> players . ix savePlayerId . playerCharacter . charQi .~ qi) saveQi
    . maybe id (\maxQi -> players . ix savePlayerId . playerCharacter . charMaxQi .~ maxQi) saveMaxQi
    . maybe id (\jing -> players . ix savePlayerId . playerCharacter . charJing .~ jing) saveJing
    . maybe id (\desc -> players . ix savePlayerId . playerCharacter . charDesc .~ desc) saveDesc
    . maybe id (\gender -> players . ix savePlayerId . playerCharacter . charGender .~ gender) saveGender
    . maybe id (\appearance -> players . ix savePlayerId . playerCharacter . charAppearance .~ clampAppearanceScore appearance) saveAppearance
    . maybe id (\innate -> players . ix savePlayerId . playerCharacter . charInnate .~ innate) saveInnate
    . (stories . at savePlayerId ?~ saveStory)

playerSaveFromGameState :: PlayerId -> GameState -> Maybe PlayerSave
playerSaveFromGameState pid gs = do
  player <- M.lookup pid (gs ^. players)
  let savePlayerId = pid
      saveVersion = 4
      saveStory = fromMaybe newPlayerStoryState $ M.lookup pid (gs ^. stories)
      saveInventory = player ^. playerInventory
      saveMoney = player ^. playerMoney
      savePotential = player ^. playerPotential
      saveCombatExp = player ^. playerCombatExp
      saveDesc = Just $ player ^. playerCharacter . charDesc
      saveHp = Just $ player ^. playerCharacter . charHP
      saveMaxHp = Just $ player ^. playerCharacter . charMaxHP
      saveQi = Just $ player ^. playerCharacter . charQi
      saveMaxQi = Just $ player ^. playerCharacter . charMaxQi
      saveJing = Just $ player ^. playerCharacter . charJing
      saveGender = Just $ player ^. playerCharacter . charGender
      saveAppearance = Just $ player ^. playerCharacter . charAppearance
      saveInnate = Just $ player ^. playerCharacter . charInnate
      saveArts = player ^. playerCharacter . charArt
      savePrepared = player ^. playerCharacter . charPrepare
      saveEnabled = player ^. playerCharacter . charEnabled
  pure PlayerSave {..}

playerSavePath :: FilePath -> PlayerId -> FilePath
playerSavePath saveDir pid = saveDir </> T.unpack (sanitizePlayerId pid) <> ".json"

sanitizePlayerId :: PlayerId -> Text
sanitizePlayerId =
  T.map $ \ch ->
    if isAlphaNum ch || ch == '-' || ch == '_' || ch == '.'
      then ch
      else '_'
