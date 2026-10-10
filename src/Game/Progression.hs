-- | Pure rules of long-term progression: level caps, proficiency, action costs
-- and battle rewards. See docs/character-progression.md for the design.
module Game.Progression where

-- | Proficiency needed to go from @targetLevel - 1@ to @targetLevel@.
artProgressRequired :: Int -> Int
artProgressRequired targetLevel = 10 * max 1 targetLevel + 20

-- | Combat experience needed before an art may reach @targetLevel@.
combatExpRequiredForLevel :: Int -> Int
combatExpRequiredForLevel targetLevel = (max 0 targetLevel ^ (3 :: Int)) `div` 10

-- | What stops an art from reaching its next level.
data LevelBlock
  = BlockedByMaxLevel
  | -- | the level that needs more combat experience
    BlockedByCombatExp Int
  | -- | the cap set by the action's source (teacher, book, foundation)
    BlockedBySourceCap Int
  deriving (Show, Eq)

-- | Why @level@ cannot advance to @level + 1@, if anything stops it.
levelBlock :: Int -> Int -> Maybe Int -> Int -> Maybe LevelBlock
levelBlock maxLevel combatExp sourceCap level
  | level >= maxLevel = Just BlockedByMaxLevel
  | combatExp < combatExpRequiredForLevel (level + 1) = Just (BlockedByCombatExp (level + 1))
  | Just cap <- sourceCap, level >= cap = Just (BlockedBySourceCap cap)
  | otherwise = Nothing

-- | Whether one more training step can still add anything. A source cap
-- (teacher, book) stops the step outright; missing combat experience still
-- lets proficiency fill up to one short of the next level, ready for later.
canGainProgress :: Int -> Int -> Maybe Int -> Int -> Int -> Maybe LevelBlock
canGainProgress maxLevel combatExp sourceCap level progress =
  case levelBlock maxLevel combatExp sourceCap level of
    Just block@(BlockedByCombatExp _)
      | progress < artProgressRequired (level + 1) - 1 -> Nothing
      | otherwise -> Just block
    other -> other

-- | Add proficiency and level up as far as the caps allow. While the next
-- level is blocked, proficiency stops one short of a full bar; at max level it
-- is cleared.
addArtProgress :: Int -> Int -> Maybe Int -> Int -> Int -> Int -> (Int, Int)
addArtProgress maxLevel combatExp sourceCap gain = go
  where
    go level progress =
      let total = progress + max 0 gain
       in settle level total
    settle level total =
      case levelBlock maxLevel combatExp sourceCap level of
        Just BlockedByMaxLevel -> (level, 0)
        Just _ -> (level, min total (artProgressRequired (level + 1) - 1))
        Nothing
          | total >= artProgressRequired (level + 1) ->
              settle (level + 1) (total - artProgressRequired (level + 1))
          | otherwise -> (level, total)

-- | Share of a specific art's proficiency that also goes to its foundation.
foundationShare :: Int -> Int
foundationShare gain = gain `div` 2

-- Action costs and gains. @level@ is the art's current level.

learnJingCost :: Int -> Int
learnJingCost level = max 5 (12 + level `div` 2)

learnProgress :: Int -> Int
learnProgress teachBonus = 12 + teachBonus

studyJingCost :: Int -> Int
studyJingCost level = max 8 (18 + level `div` 2)

studyProgress :: Int -> Int
studyProgress quality = 10 + 2 * quality

practiceJingCost :: Int -> Int
practiceJingCost level = max 5 (6 + level `div` 4)

practiceHpCost :: Int -> Int -> Int
practiceHpCost level vitality = max 0 (8 + level `div` 2 - vitality `div` 4)

practiceQiCost :: Int -> Int
practiceQiCost level = 10 + level `div` 2

practiceProgress :: Int -> Int
practiceProgress foundationLevel = 4 + foundationLevel `div` 10

researchJingCost :: Int -> Int
researchJingCost level = max 10 (20 + level)

researchProgress :: Int
researchProgress = 20

meditateJingCost :: Int -> Int
meditateJingCost amount = max 5 (amount `div` 10)

meditateMaxQiGain :: Int -> Int -> Int
meditateMaxQiGain amount internalPower = max 1 (amount `div` 20 + internalPower `div` 50)

-- | Long-term max Qi that meditation can build, set by internal arts.
storedMaxQiCap :: Int -> Int -> Int
storedMaxQiCap basicInternal enabledInternal = 100 + 10 * (basicInternal + enabledInternal)

-- Busy seconds per step.

learnBusySeconds, studyBusySeconds, practiceBusySeconds, researchBusySeconds :: Double
learnBusySeconds = 2
studyBusySeconds = 3
practiceBusySeconds = 3
researchBusySeconds = 4

meditateBusySeconds :: Int -> Double
meditateBusySeconds amount = max 2 (min 10 (fromIntegral amount / 20))

-- Battle rewards.

-- | Combat experience for beating an opponent of the same rank.
baseCombatExp :: Int -> Int
baseCombatExp rank = 3 + 7 * max 0 rank `div` 10

-- | Reward multiplier by how much stronger the enemy ranks than the player.
rewardScale :: Int -> Double
rewardScale diff
  | diff <= -10 = 0.1
  | diff <= -4 = 0.5
  | diff <= 3 = 1.0
  | diff <= 10 = 1.3
  | otherwise = 0.6

battleCombatExpGain :: Int -> Int -> Int
battleCombatExpGain playerRank enemyRank =
  floor (fromIntegral (baseCombatExp enemyRank) * rewardScale (enemyRank - playerRank) :: Double)

battlePotentialGain :: Int -> Int
battlePotentialGain combatExpGain
  | combatExpGain <= 0 = 0
  | otherwise = max 1 (combatExpGain `div` 2)

-- | Potential above this is not kept, so players spend it instead of hoarding.
potentialCap :: Int -> Int
potentialCap combatExp = 200 + combatExp `div` 10
