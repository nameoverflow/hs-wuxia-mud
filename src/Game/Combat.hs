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
import Control.Monad (forM_, when)
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

data BattleState = BattleState
  { _battleSkillCd :: M.Map SkillId Double,
    _battleAp :: Int,
    _battleQi :: Int,
    _battleChar :: Character,
    _battleEffects :: M.Map EffectId ActiveEffect
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
          _battleQi = _charQi char,
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

  -- Update skill cooldowns
  battleState . battleSkillCd %= M.filter (> 0.0) . M.map (subtract dt)
  battleEnemyState . battleSkillCd %= M.filter (> 0.0) . M.map (subtract dt)

  -- Update active effects (reduce duration, remove expired)
  battleState . battleEffects %= M.filter ((> 0.0) . _activeEffectRemaining) . M.map (\e -> e & activeEffectRemaining %~ subtract dt)
  battleEnemyState . battleEffects %= M.filter ((> 0.0) . _activeEffectRemaining) . M.map (\e -> e & activeEffectRemaining %~ subtract dt)

  -- Regenerate Qi
  battleState . battleQi += round (dt * _charQiRegen char)
  battleEnemyState . battleQi += round (dt * _charQiRegen enemy)

  -- Cap Qi at max
  battleState . battleQi %= min (_charMaxQi char)
  battleEnemyState . battleQi %= min (_charMaxQi enemy)

  -- Accumulate action points
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

-- | Check if all requirements are met to cast a skill
canCastSkill :: Skill -> Lens' Battle BattleState -> Combat Bool
canCastSkill skill state = do
  apValue <- use $ state . battleAp
  qi <- use $ state . battleQi
  cds <- use $ state . battleSkillCd
  effects <- use $ state . battleEffects

  let hasEnoughAp = apValue >= skill ^. skillApReq
      hasEnoughQi = qi >= skill ^. skillCost
      isOffCooldown = not $ M.member (skill ^. skillId) cds
      hasRequiredEffects = all (`M.member` effects) (skill ^. skillReqStatus)

  return $ hasEnoughAp && hasEnoughQi && isOffCooldown && hasRequiredEffects

-- | Cast a skill and apply its effects immediately
castSkill :: Skill -> Lens' Battle BattleState -> Lens' Battle BattleState -> Combat ()
castSkill skill caster target = do
  -- Consume Qi
  caster . battleQi -= skill ^. skillCost

  -- Set cooldown
  caster . battleSkillCd . at (skill ^. skillId) .= Just (skill ^. skillCooldown)

  -- Apply effects based on target type
  case skill ^. skillTarget of
    Single -> applySkillEffects skill caster target
    Self   -> applySkillEffects skill caster caster
    All    -> applySkillEffects skill caster target  -- For now, same as Single

  -- Generate skill message
  uid <- use battleOwner
  casterName <- use $ caster . battleChar . charName
  targetName <- use $ target . battleChar . charName
  tell [(uid, SkillMsg casterName targetName (skill ^. skillMsg))]

-- | Apply damage, healing, and status effects from a skill
applySkillEffects :: Skill -> Lens' Battle BattleState -> Lens' Battle BattleState -> Combat ()
applySkillEffects skill caster target = do
  uid <- use battleOwner
  casterName <- use $ caster . battleChar . charName
  targetName <- use $ target . battleChar . charName

  -- Apply damage
  case skill ^. skillDamage of
    Just dmg -> do
      target . battleChar . charHP -= dmg
      tell [(uid, CombatNormalMsg casterName targetName (skill ^. skillMsg) dmg)]
    Nothing -> return ()

  -- Apply healing
  case skill ^. skillHeal of
    Just heal -> do
      target . battleChar . charHP += heal
      -- TODO: Add proper healing message
    Nothing -> return ()

  -- Apply effects to target
  forM_ (skill ^. skillEffTarget) $ \(effId, duration, value) -> do
    let activeEff = ActiveEffect
          { _activeEffectDef = effId,
            _activeEffectRemaining = duration,
            _activeEffectValue = value
          }
    target . battleEffects . at effId .= Just activeEff

  -- Apply effects to caster (self-buffs)
  forM_ (skill ^. skillEffSelf) $ \(effId, duration, value) -> do
    let activeEff = ActiveEffect
          { _activeEffectDef = effId,
            _activeEffectRemaining = duration,
            _activeEffectValue = value
          }
    caster . battleEffects . at effId .= Just activeEff
