{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game.CharacterCreation
  ( CharacterCreationChoice (..),
    applyCharacterCreationChoice,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Game.Entity

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

applyCharacterCreationChoice :: CharacterCreationChoice -> Player -> Player
applyCharacterCreationChoice choice player =
  restoreVitals $
    player
      & playerCharacter . charInnate . innateStrength %~ addClamped (bonusStrength totalBonus)
      & playerCharacter . charInnate . innateAgility %~ addClamped (bonusAgility totalBonus)
      & playerCharacter . charInnate . innateVitality %~ addClamped (bonusVitality totalBonus)
      & playerCharacter . charMaxQi %~ max 0 . (+ bonusMaxQi totalBonus)
      & playerCharacter . charAppearance %~ clampAppearanceScore . (+ bonusAppearance totalBonus)
      & playerCharacter . charDesc .~ characterCreationSummary choice
  where
    totalBonus =
      originBonus (creationOrigin choice)
        `appendBonus` childhoodBonus (creationChildhoodOne choice)
        `appendBonus` childhoodBonus (creationChildhoodTwo choice)

    addClamped delta = clampInnateScore . (+ delta)

restoreVitals :: Player -> Player
restoreVitals player =
  let derived = deriveStats player
   in player
        & playerCharacter . charHP .~ (derived ^. dsMaxHp)
        & playerCharacter . charMaxHP .~ (derived ^. dsMaxHp)
        & playerCharacter . charQi .~ (derived ^. dsMaxQi)
        & playerCharacter . charJing .~ (derived ^. dsMaxJing)

originBonus :: Text -> CreationBonus
originBonus = \case
  "martial_family" -> CreationBonus 3 1 1 12 0
  "scholar_house" -> CreationBonus 0 2 2 8 1
  "official_house" -> CreationBonus 1 1 3 0 1
  "medicine_house" -> CreationBonus 0 1 3 8 1
  "orphan" -> CreationBonus 2 3 0 0 (-1)
  _ -> emptyBonus

childhoodBonus :: Text -> CreationBonus
childhoodBonus = \case
  "courtyard_practice" -> CreationBonus 2 1 0 0 0
  "river_chase" -> CreationBonus 0 3 0 0 1
  "herb_gathering" -> CreationBonus 0 0 3 0 0
  "night_reading" -> CreationBonus 0 1 2 0 1
  "market_brawls" -> CreationBonus 2 0 1 0 (-1)
  "mountain_errands" -> CreationBonus 1 1 1 0 0
  "breath_lessons" -> CreationBonus 0 0 1 12 1
  "cold_watch" -> CreationBonus 1 0 2 0 0
  _ -> emptyBonus

characterCreationSummary :: CharacterCreationChoice -> Text
characterCreationSummary CharacterCreationChoice {..} =
  T.intercalate
    "\n"
    [ "我出身于" <> optionName creationOrigin <> "。",
      "幼年时，" <> optionName creationChildhoodOne <> "。",
      "后来，" <> optionName creationChildhoodTwo <> "。"
    ]

optionName :: Text -> Text
optionName = \case
  "martial_family" -> "武学世家"
  "scholar_house" -> "书香门第"
  "official_house" -> "官宦人家"
  "medicine_house" -> "医药之家"
  "orphan" -> "无亲孤儿"
  "courtyard_practice" -> "我常在院中偷练拳脚"
  "river_chase" -> "我追着渡船和流云奔跑"
  "herb_gathering" -> "我随长辈入山辨草采药"
  "night_reading" -> "我伴着灯火读旧书"
  "market_brawls" -> "我在市井里学会挨打和还手"
  "mountain_errands" -> "我替人翻山送信取物"
  "breath_lessons" -> "我记住了几句调息口诀"
  "cold_watch" -> "我在寒夜里守过长门"
  other -> other
