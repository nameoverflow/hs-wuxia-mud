{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game.CharacterCreation
  ( CharacterCreationChoice (..),
    CharacterCreationConfig (..),
    CreationBonus (..),
    CreationOption (..),
    applyCharacterCreationChoice,
    characterCreationConfigPath,
    loadCharacterCreationConfig,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Game.Entity
import Utils

data CharacterCreationChoice = CharacterCreationChoice
  { creationOrigin :: Text,
    creationChildhoodOne :: Text,
    creationChildhoodTwo :: Text
  }
  deriving (Show, Eq)

instance FromJSON CharacterCreationChoice where
  parseJSON = withObject "CharacterCreationChoice" $ \o ->
    CharacterCreationChoice
      <$> o .: "origin"
      <*> o .: "childhood1"
      <*> o .: "childhood2"

instance ToJSON CharacterCreationChoice where
  toJSON CharacterCreationChoice {..} =
    object
      [ "origin" .= creationOrigin,
        "childhood1" .= creationChildhoodOne,
        "childhood2" .= creationChildhoodTwo
      ]

data CreationBonus = CreationBonus
  { bonusStrength :: Int,
    bonusAgility :: Int,
    bonusVitality :: Int,
    bonusMaxQi :: Int,
    bonusAppearance :: Int
  }
  deriving (Show, Eq)

instance FromJSON CreationBonus where
  parseJSON = withObject "CreationBonus" $ \o ->
    CreationBonus
      <$> o .:? "strength" .!= 0
      <*> o .:? "agility" .!= 0
      <*> o .:? "vitality" .!= 0
      <*> o .:? "maxQi" .!= 0
      <*> o .:? "appearance" .!= 0

instance ToJSON CreationBonus where
  toJSON bonus =
    object
      [ "strength" .= bonusStrength bonus,
        "agility" .= bonusAgility bonus,
        "vitality" .= bonusVitality bonus,
        "maxQi" .= bonusMaxQi bonus,
        "appearance" .= bonusAppearance bonus
      ]

data CreationOption = CreationOption
  { creationOptionId :: Text,
    creationOptionLabel :: Text,
    creationOptionStory :: Text,
    creationOptionBonus :: CreationBonus
  }
  deriving (Show, Eq)

instance FromJSON CreationOption where
  parseJSON = withObject "CreationOption" $ \o ->
    CreationOption
      <$> o .: "id"
      <*> o .: "label"
      <*> o .: "story"
      <*> o .: "bonus"

instance ToJSON CreationOption where
  toJSON option =
    object
      [ "id" .= creationOptionId option,
        "label" .= creationOptionLabel option,
        "story" .= creationOptionStory option,
        "bonus" .= creationOptionBonus option
      ]

data CharacterCreationConfig = CharacterCreationConfig
  { creationBaseStats :: CreationBonus,
    creationOrigins :: [CreationOption],
    creationChildhoodOneOptions :: [CreationOption],
    creationChildhoodTwoOptions :: [CreationOption]
  }
  deriving (Show, Eq)

instance FromJSON CharacterCreationConfig where
  parseJSON = withObject "CharacterCreationConfig" $ \o ->
    CharacterCreationConfig
      <$> o .: "baseStats"
      <*> o .: "origins"
      <*> o .: "childhood1"
      <*> o .: "childhood2"

instance ToJSON CharacterCreationConfig where
  toJSON config =
    object
      [ "baseStats" .= creationBaseStats config,
        "origins" .= creationOrigins config,
        "childhood1" .= creationChildhoodOneOptions config,
        "childhood2" .= creationChildhoodTwoOptions config
      ]

instance Configurable CharacterCreationConfig

characterCreationConfigPath :: FilePath
characterCreationConfigPath = "resources/scripts/character_creation.yaml"

loadCharacterCreationConfig :: FilePath -> IO (Either Text CharacterCreationConfig)
loadCharacterCreationConfig = loadConfigFrom

emptyBonus :: CreationBonus
emptyBonus = CreationBonus 0 0 0 0 0

appendBonus :: CreationBonus -> CreationBonus -> CreationBonus
appendBonus a b =
  CreationBonus
    { bonusStrength = bonusStrength a + bonusStrength b,
      bonusAgility = bonusAgility a + bonusAgility b,
      bonusVitality = bonusVitality a + bonusVitality b,
      bonusMaxQi = bonusMaxQi a + bonusMaxQi b,
      bonusAppearance = bonusAppearance a + bonusAppearance b
    }

applyCharacterCreationChoice :: CharacterCreationConfig -> CharacterCreationChoice -> Player -> Player
applyCharacterCreationChoice config choice player =
  restoreVitals $
    player
      & playerCharacter . charInnate . innateStrength %~ addClamped (bonusStrength totalBonus)
      & playerCharacter . charInnate . innateAgility %~ addClamped (bonusAgility totalBonus)
      & playerCharacter . charInnate . innateVitality %~ addClamped (bonusVitality totalBonus)
      & playerCharacter . charMaxQi %~ max 0 . (+ bonusMaxQi totalBonus)
      & playerCharacter . charAppearance %~ clampAppearanceScore . (+ bonusAppearance totalBonus)
      & playerCharacter . charDesc .~ characterCreationSummary config choice
  where
    totalBonus =
      optionBonus (creationOrigins config) (creationOrigin choice)
        `appendBonus` optionBonus (creationChildhoodOneOptions config) (creationChildhoodOne choice)
        `appendBonus` optionBonus (creationChildhoodTwoOptions config) (creationChildhoodTwo choice)

    addClamped delta = clampInnateScore . (+ delta)

restoreVitals :: Player -> Player
restoreVitals player =
  let derived = deriveStats player
   in player
        & playerCharacter . charHP .~ (derived ^. dsMaxHp)
        & playerCharacter . charMaxHP .~ (derived ^. dsMaxHp)
        & playerCharacter . charQi .~ (derived ^. dsMaxQi)
        & playerCharacter . charJing .~ (derived ^. dsMaxJing)

optionBonus :: [CreationOption] -> Text -> CreationBonus
optionBonus options optionId =
  maybe emptyBonus creationOptionBonus $ findOption options optionId

optionName :: [CreationOption] -> Text -> Text
optionName options optionId =
  maybe optionId creationOptionLabel $ findOption options optionId

findOption :: [CreationOption] -> Text -> Maybe CreationOption
findOption options optionId =
  case filter ((== optionId) . creationOptionId) options of
    option : _ -> Just option
    [] -> Nothing

characterCreationSummary :: CharacterCreationConfig -> CharacterCreationChoice -> Text
characterCreationSummary config CharacterCreationChoice {..} =
  T.intercalate
    "\n"
    [ "我出身于" <> optionName (creationOrigins config) creationOrigin <> "。",
      "幼年时，" <> optionName (creationChildhoodOneOptions config) creationChildhoodOne <> "。",
      "后来，" <> optionName (creationChildhoodTwoOptions config) creationChildhoodTwo <> "。"
    ]
