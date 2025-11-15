{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Combat where

import Control.Exception (Exception)
import Control.Lens
import Control.Monad (when)
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.RWS (RWST (..), MonadWriter (tell))
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State.Strict (MonadState, StateT (..))
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Game.Entity
import Game.Message
import Game.World (World, skills)
import Utils
import Relude (ToText (..), whenNothing)

type BattleId = Text

maxAp :: Int
maxAp = 100

data StatusEffect = StatusEffect
  { _effectId :: EffectId,
    _effectName :: Text,
    _effectDuration :: Double,
    _effectType :: EffectType
  }
  deriving (Show, Eq, Generic)

data EffectType = DoT Int | HoT Int | Buff Int | DeBuff Int
  deriving (Show, Eq, Generic)

makeLenses ''StatusEffect

data BattleState = BattleState
  { _battleSkillCd :: M.Map SkillId Double,
    _battleAp :: Int,
    _battleChar :: Character,
    _battleEffects :: M.Map EffectId StatusEffect
  }
  deriving (Show, Eq, Generic)

data Battle = Battle
  { _battleOwner :: PlayerId,
    _battleState :: !BattleState,
    _battleEnemyState :: !BattleState
  }
  deriving (Show, Eq, Generic)

makeLenses ''Battle

makeLenses ''BattleState

newBattle :: Player -> Character -> Battle
newBattle player npc =
  Battle
    { _battleOwner = _playerId player,
      _battleState = newBattleState $ _playerCharacter player,
      _battleEnemyState = newBattleState npc
    }
  where
    newBattleState char =
      BattleState
        { _battleSkillCd = M.empty,
          _battleAp = 0,
          _battleChar = char,
          _battleEffects = M.empty
        }

data CombatException = CombatException Text
  deriving (Show, Eq, Generic)

instance Exception CombatException
instance ToText CombatException where
  toText (CombatException msg) = msg

newtype Combat a = Combat
  { unCombat :: RWST World [PlayerResp] Battle (ExceptT CombatException (Rand StdGen)) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState Battle,
      MonadReader World,
      MonadError CombatException,
      MonadRandom,
      MonadWriter [PlayerResp]
    )

runCombat :: (MonadError e m) => StdGen -> (CombatException -> e) -> World -> Battle -> Combat a -> m (a, Battle, [PlayerResp])
runCombat rand fCatch world battle combat = do
  let (action, _) = runRand (runExceptT (runRWST (unCombat combat) world battle)) rand
  liftEither $ either (Left . fCatch) Right action

-- | Run a combat action, return if the battle is over
flushBattleTick :: Double -> Combat Bool
flushBattleTick dt = do
  char <- use $ battleState . battleChar
  enemy <- use $ battleEnemyState . battleChar
  battleState . battleSkillCd %= M.filter (> 0.0) . M.map (subtract dt)
  battleEnemyState . battleSkillCd %= M.filter (> 0.0) . M.map (subtract dt)

  -- action points moving on
  battleState . battleAp += round (dt * fromIntegral (_charAgility char))
  battleEnemyState . battleAp += round (dt * fromIntegral (_charAgility enemy))

  -- Check whether to attack the enemy
  checkApAndAttack battleState battleEnemyState
  enemyHp <- use $ battleEnemyState . battleChar . charHP
  if enemyHp <= 0
    then return True
    else do
      checkApAndAttack battleEnemyState battleState
      playerHp <- use $ battleState . battleChar . charHP
      return $ playerHp <= 0

checkApAndAttack :: Lens' Battle BattleState -> Lens' Battle BattleState -> Combat ()
checkApAndAttack left right = do
  leftAp <- use $ left . battleAp
  when (leftAp >= maxAp) $ do
    left . battleAp .= 0
    (left . battleChar) `battleAttack` (right . battleChar)

-- | Random select a move from player's skill to attack
battleAttack :: Lens' Battle Character -> Lens' Battle Character -> Combat ()
battleAttack left right = do
  eqTechId <- use $ left . charPrepare . ix Technique . artDef
  eqMoves <- view $ skills . at eqTechId . _Just . artMoves

  -- randomly select a move
  move <- randomSelect eqMoves
  case move of
    Nothing -> do
      c <- view $ skills . at eqTechId
      throwError $ CombatException $ pack $ "No move selected, skill: " <> show c
    Just mv -> do
      -- apply damage
      let damage = mv ^. moveDamage
      right . charHP -= damage

      -- write attack message
      uid <- use battleOwner
      leftName <- use $ left . charName
      rightName <- use $ right . charName
      let mvMsg = mv ^. moveMsg
      tell [(uid, CombatNormalMsg leftName rightName mvMsg damage)]

-- | Use a skill in combat (player action)
useSkill :: SkillId -> Combat ()
useSkill skillId = do
  -- Get the skill definition from world
  maybeArt <- getPlayerSkillArt skillId
  case maybeArt of
    Nothing -> throwError $ CombatException $ "You don't have access to skill: " <> skillId
    Just art -> do
      let maybeSkill = findSkillInArt skillId art
      case maybeSkill of
        Nothing -> throwError $ CombatException $ "Skill not found in martial art: " <> skillId
        Just skill -> do
          -- Validate skill usage
          canUse <- canUseSkill skill
          case canUse of
            Left err -> throwError $ CombatException err
            Right () -> do
              -- Deduct AP cost
              battleState . battleAp -= (skill ^. skillCost)

              -- Set cooldown
              battleState . battleSkillCd . at skillId .= Just (skill ^. skillCooldown)

              -- Apply skill effects
              applySkillEffect skill battleState battleEnemyState

              -- Send skill message
              uid <- use battleOwner
              playerName <- use $ battleState . battleChar . charName
              enemyName <- use $ battleEnemyState . battleChar . charName
              let msg = skill ^. skillMsg
              tell [(uid, SkillMsg playerName enemyName msg)]

-- | Find the martial art that contains this skill (from player's prepared arts)
getPlayerSkillArt :: SkillId -> Combat (Maybe MartialArt)
getPlayerSkillArt skillId = do
  char <- use $ battleState . battleChar
  let preparedArts = M.elems $ char ^. charPrepare

  -- Look up each prepared art and check if it contains the skill
  arts <- mapM (\artEntity -> view $ skills . at (artEntity ^. artDef)) preparedArts

  -- Find the first art that contains this skill
  return $ findArtWithSkill skillId (catMaybes arts)
  where
    findArtWithSkill :: SkillId -> [MartialArt] -> Maybe MartialArt
    findArtWithSkill sid arts =
      case filter (hasSkill sid) arts of
        (art:_) -> Just art
        [] -> Nothing

    hasSkill :: SkillId -> MartialArt -> Bool
    hasSkill sid art = any (\s -> s ^. skillId == sid) (art ^. artSkills)

-- | Find a specific skill within a martial art
findSkillInArt :: SkillId -> MartialArt -> Maybe Skill
findSkillInArt sid art =
  case filter (\s -> s ^. skillId == sid) (art ^. artSkills) of
    (skill:_) -> Just skill
    [] -> Nothing

-- | Check if a skill can be used (cooldown and AP validation)
canUseSkill :: Skill -> Combat (Either Text ())
canUseSkill skill = do
  let sid = skill ^. skillId
  let cost = skill ^. skillCost

  -- Check cooldown
  cooldowns <- use $ battleState . battleSkillCd
  case M.lookup sid cooldowns of
    Just cd | cd > 0 -> return $ Left $ "Skill " <> (skill ^. skillName) <> " is on cooldown (" <> pack (show (round cd :: Int)) <> "s remaining)"
    _ -> do
      -- Check AP
      currentAp <- use $ battleState . battleAp
      if currentAp < cost
        then return $ Left $ "Not enough AP. Need " <> pack (show cost) <> ", have " <> pack (show currentAp)
        else return $ Right ()

-- | Apply the effects of a skill (damage, heal, status effects)
applySkillEffect :: Skill -> Lens' Battle BattleState -> Lens' Battle BattleState -> Combat ()
applySkillEffect skill casterLens targetLens = do
  uid <- use battleOwner
  casterName <- use $ casterLens . battleChar . charName
  targetName <- use $ targetLens . battleChar . charName

  let target = skill ^. skillTarget

  -- Apply based on target type
  case target of
    Self -> applySingleEffect skill casterLens casterName uid
    Single -> applySingleEffect skill targetLens targetName uid
    All -> do
      -- Apply to both (for now, in single combat this is same as Single)
      applySingleEffect skill targetLens targetName uid

  where
    applySingleEffect :: Skill -> Lens' Battle BattleState -> Text -> PlayerId -> Combat ()
    applySingleEffect sk targetL targetN uid = do
      -- Apply damage
      case sk ^. skillDamage of
        Just dmg -> do
          targetL . battleChar . charHP -= dmg
          tell [(uid, CombatNormalMsg casterName targetN (sk ^. skillMsg) dmg)]
        Nothing -> return ()

      -- Apply healing
      case sk ^. skillHeal of
        Just heal -> do
          targetL . battleChar . charHP += heal
          tell [(uid, CombatNormalMsg casterName targetN (sk ^. skillMsg <> " (healed)") heal)]
        Nothing -> return ()

      -- TODO: Apply status effects from skillEffSelf and skillEffTarget
      -- For now, status effects are not implemented in the YAML files
