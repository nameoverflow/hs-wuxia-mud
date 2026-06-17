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
import qualified Data.Set as S
import Data.Text (Text, pack)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Game.Entity
import Game.Message
import Game.World (World, effects, martialArts)
import Utils
import Relude (ToText (..), whenNothing)

type BattleId = Text

maxAp :: Int
maxAp = 100

targetCombatantActionSeconds :: Double
targetCombatantActionSeconds = 2.0

baselineAgility :: Double
baselineAgility = 19.0

apGainRate :: Double
apGainRate = fromIntegral maxAp / (targetCombatantActionSeconds * baselineAgility)

data BattleState = BattleState
  { _battleActiveSkillCooldowns :: M.Map ActiveSkillId Double,
    _battleAp :: Int,
    _battleQi :: Int,
    _battleCombatExp :: Int,
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

data PreparedAttack = PreparedAttack
  { preparedAttackArt :: ArtEntity,
    preparedAttackMartialArt :: MartialArt,
    preparedAttackMove :: AttackMove
  }
  deriving (Show, Eq, Generic)

makeLenses ''Battle

makeLenses ''BattleState

newBattle :: Player -> Character -> Battle
newBattle player npc =
  Battle
    { _battleOwner = _playerId player,
      _battleState = newBattleState (player ^. playerCharacter) (player ^. playerCombatExp),
      _battleEnemyState = newBattleState npc 0
    }
  where
    newBattleState char combatExp =
      BattleState
        { _battleActiveSkillCooldowns = M.empty,
          _battleAp = 0,
          _battleQi = _charQi char,
          _battleCombatExp = combatExp,
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
  -- Update active skill cooldowns
  battleState . battleActiveSkillCooldowns %= M.filter (> 0.0) . M.map (subtract dt)
  battleEnemyState . battleActiveSkillCooldowns %= M.filter (> 0.0) . M.map (subtract dt)

  -- Apply and expire active effects.
  applyActiveEffects dt battleState
  applyActiveEffects dt battleEnemyState
  expireActiveEffects dt battleState
  expireActiveEffects dt battleEnemyState

  playerHpAfterEffects <- use $ battleState . battleChar . charHP
  enemyHpAfterEffects <- use $ battleEnemyState . battleChar . charHP
  if playerHpAfterEffects <= 0 || enemyHpAfterEffects <= 0
    then return True
    else do
      playerStats <- battleDerivedStats battleState
      enemyStats <- battleDerivedStats battleEnemyState

      -- Regenerate Qi
      battleState . battleQi += round (dt * (playerStats ^. dsQiRegen))
      battleEnemyState . battleQi += round (dt * (enemyStats ^. dsQiRegen))

      -- Cap Qi at max
      battleState . battleQi %= min (playerStats ^. dsMaxQi)
      battleEnemyState . battleQi %= min (enemyStats ^. dsMaxQi)

      -- Accumulate action points
      battleState . battleAp += apGain dt playerStats
      battleEnemyState . battleAp += apGain dt enemyStats

      -- Check whether to attack the enemy
      checkApAndAttack battleState battleEnemyState
      enemyHp <- use $ battleEnemyState . battleChar . charHP
      if enemyHp <= 0
        then return True
        else do
          checkApAndAttack battleEnemyState battleState
          playerHp <- use $ battleState . battleChar . charHP
          return $ playerHp <= 0

applyActiveEffects :: Double -> Lens' Battle BattleState -> Combat ()
applyActiveEffects dt state = do
  active <- use $ state . battleEffects
  effectDefs <- view effects
  uid <- use battleOwner
  targetName <- use $ state . battleChar . charName
  forM_ active $ \activeEffect -> do
    let amount = max 0 . round $ dt * fromIntegral (activeEffect ^. activeEffectValue)
    case M.lookup (activeEffect ^. activeEffectDef) effectDefs of
      Just effect
        | amount > 0 ->
            case effect ^. effectType of
              DoT -> do
                state . battleChar . charHP -= amount
                tell
                  [
                    ( uid,
                      combatEventResp
                        CombatEventEffectTick
                        (effect ^. effectName)
                        targetName
                        (CombatEffectTick (activeEffect ^. activeEffectDef) (effect ^. effectName) "dot" amount)
                        (Just amount)
                        Nothing
                        CombatHit
                        (effectTickVisual "dot")
                    )
                  ]
              HoT -> do
                maxHp <- (^. dsMaxHp) <$> battleDerivedStats state
                state . battleChar . charHP %= min maxHp . (+ amount)
                tell
                  [
                    ( uid,
                      combatEventResp
                        CombatEventEffectTick
                        (effect ^. effectName)
                        targetName
                        (CombatEffectTick (activeEffect ^. activeEffectDef) (effect ^. effectName) "hot" amount)
                        Nothing
                        (Just amount)
                        CombatEffect
                        (effectTickVisual "hot")
                    )
                  ]
              Buff -> return ()
              DeBuff -> return ()
      _ -> return ()

expireActiveEffects :: Double -> Lens' Battle BattleState -> Combat ()
expireActiveEffects dt state =
  state . battleEffects %= M.filter ((> 0.0) . _activeEffectRemaining) . M.map (\e -> e & activeEffectRemaining %~ subtract dt)

battleDerivedStats :: Lens' Battle BattleState -> Combat DerivedStats
battleDerivedStats state = do
  char <- use $ state . battleChar
  combatExp <- use $ state . battleCombatExp
  activeEffects <- use $ state . battleEffects
  effectDefs <- view effects
  pure $ deriveCharacterStats char combatExp (characterDerivedStatSources effectDefs activeEffects char)

checkApAndAttack :: Lens' Battle BattleState -> Lens' Battle BattleState -> Combat ()
checkApAndAttack left right = do
  leftAp <- use $ left . battleAp
  when (leftAp >= maxAp) $ do
    left . battleAp .= 0
    left `battleAttack` right

apGain :: Double -> DerivedStats -> Int
apGain dt stats =
  round $ dt * fromIntegral (stats ^. dsAgility) * apGainRate

combatEventResp :: CombatEventKind -> Text -> Text -> CombatMessage -> Maybe Int -> Maybe Int -> CombatResult -> CombatVisualHint -> ActionResp
combatEventResp kind actorName targetName message damage heal result visual =
  CombatEventMsg $
    CombatEvent
      { combatEventKind = kind,
        combatEventActorName = actorName,
        combatEventTargetName = targetName,
        combatEventMessage = message,
        combatEventDamage = damage,
        combatEventHeal = heal,
        combatEventResult = result,
        combatEventVisual = visual
      }

effectTickVisual :: Text -> CombatVisualHint
effectTickVisual effectKind =
  CombatVisualHint
    { _combatVisualActionId = "rig.effect." <> effectKind,
      _combatVisualTags = ["effect", effectKind]
    }

resolveAnimationRef :: MartialArt -> AnimationRef -> Combat CombatVisualHint
resolveAnimationRef martialArt animationRef =
  case animationRef ^. animationRefAction of
    Just actionId ->
      pure $
        CombatVisualHint
          { _combatVisualActionId = actionId,
            _combatVisualTags = animationRef ^. animationRefTags
          }
    Nothing ->
      case animationRef ^. animationRefPool of
        Nothing -> throwError $ CombatException "animation must define action or pool"
        Just poolId ->
          case martialArt ^. artAnimationPools . at poolId of
            Nothing -> throwError $ CombatException $ "missing animation pool " <> poolId <> " in martial art " <> martialArt ^. artId
            Just pool -> do
              entry <- selectAnimationPoolEntry (animationRef ^. animationRefTags) pool
              pure $
                CombatVisualHint
                  { _combatVisualActionId = entry ^. animationPoolEntryAction,
                    _combatVisualTags = animationRef ^. animationRefTags <> entry ^. animationPoolEntryTags
                  }

selectAnimationPoolEntry :: [Text] -> AnimationPool -> Combat AnimationPoolEntry
selectAnimationPoolEntry requestedTags pool = do
  let entries = pool ^. animationPoolActions
  case entries of
    [] -> throwError $ CombatException "animation pool has no actions"
    _ -> do
      selected <- weightedSelect $ bestTaggedEntries requestedTags entries
      case selected of
        Just entry -> pure entry
        Nothing -> throwError $ CombatException "animation pool has no positive-weight actions"

bestTaggedEntries :: [Text] -> [AnimationPoolEntry] -> [AnimationPoolEntry]
bestTaggedEntries requestedTags entries =
  case scored of
    [] -> []
    _ ->
      let bestScore = maximum $ map fst scored
       in [entry | (score, entry) <- scored, score == bestScore]
  where
    requested = S.fromList $ map T.toLower requestedTags
    scored =
      [ (length $ filter (`S.member` requested) (map T.toLower $ entry ^. animationPoolEntryTags), entry)
        | entry <- entries
      ]

weightedSelect :: [AnimationPoolEntry] -> Combat (Maybe AnimationPoolEntry)
weightedSelect entries =
  case [(max 0 $ entry ^. animationPoolEntryWeight, entry) | entry <- entries, entry ^. animationPoolEntryWeight > 0] of
    [] -> pure Nothing
    weighted -> do
      let total = sum $ map fst weighted
      roll <- getRandomR (1, total)
      pure $ pickWeighted roll weighted
  where
    pickWeighted _ [] = Nothing
    pickWeighted cursor ((weight, entry) : rest)
      | cursor <= weight = Just entry
      | otherwise = pickWeighted (cursor - weight) rest

battleAttack :: Lens' Battle BattleState -> Lens' Battle BattleState -> Combat ()
battleAttack left right = do
  attacker <- use $ left . battleChar
  defender <- use $ right . battleChar
  attack <- selectPreparedAttack attacker
  case attack of
    Nothing -> do
      throwError $ CombatException $ pack $ "No unlocked move selected, prepared arts: " <> show (attacker ^. charPrepare)
    Just preparedAttack -> runAttackPipeline left right preparedAttack attacker defender

runAttackPipeline :: Lens' Battle BattleState -> Lens' Battle BattleState -> PreparedAttack -> Character -> Character -> Combat ()
runAttackPipeline left right preparedAttack attacker defender = do
  attackerStats <- battleDerivedStats left
  defenderStats <- battleDerivedStats right
  let attackScore = attackPower attackerStats preparedAttack
      dodgeScore = dodgePower defenderStats
      parryScore = parryPower defenderStats
      move = preparedAttackMove preparedAttack
      moveText = move ^. attackMoveMsg
  visual <- resolveAnimationRef (preparedAttackMartialArt preparedAttack) (move ^. attackMoveAnimation)
  hit <- contest attackScore dodgeScore
  uid <- use battleOwner
  attackerName <- use $ left . battleChar . charName
  defenderName <- use $ right . battleChar . charName
  if not hit
    then
      tell
        [ ( uid,
            combatEventResp
              CombatEventNormal
              attackerName
              defenderName
              (CombatScriptText $ moveText <> "，却被侧身闪避")
              (Just 0)
              Nothing
              CombatDodge
              visual
          )
        ]
    else do
      parryFailed <- contest attackScore parryScore
      if not parryFailed
        then
          tell
            [ ( uid,
                combatEventResp
                  CombatEventNormal
                  attackerName
                  defenderName
                  (CombatScriptText $ moveText <> "，被抬手格开")
                  (Just 0)
                  Nothing
                  CombatParry
                  visual
              )
            ]
        else do
          let damage = applyCombatHooks attacker defender preparedAttack $ computeDamage attackerStats defenderStats preparedAttack
          right . battleChar . charHP -= damage
          tell
            [ ( uid,
                combatEventResp
                  CombatEventNormal
                  attackerName
                  defenderName
                  (CombatScriptText moveText)
                  (Just damage)
                  Nothing
                  CombatHit
                  visual
              )
            ]

selectPreparedAttack :: Character -> Combat (Maybe PreparedAttack)
selectPreparedAttack char = do
  martialArtMap <- view martialArts
  randomSelect $ unlockedPreparedAttacks martialArtMap char

unlockedPreparedAttacks :: M.Map ArtId MartialArt -> Character -> [PreparedAttack]
unlockedPreparedAttacks martialArtMap char =
  [ PreparedAttack artEntity martialArt move
    | artType' <- [Sword, Fist],
      Just artEntity <- [char ^. charPrepare . at artType'],
      Just martialArt <- [M.lookup (artEntity ^. artDef) martialArtMap],
      move <- martialArt ^. artAttackMoves,
      move ^. attackMoveUnlockLevel <= artEntity ^. artLevel
  ]

contest :: Int -> Int -> Combat Bool
contest attack defense
  | attack <= 0 = return False
  | defense <= 0 = return True
  | otherwise = do
      roll <- getRandomR (1, attack + defense)
      return $ roll <= attack

attackPower :: DerivedStats -> PreparedAttack -> Int
attackPower attackerStats preparedAttack =
  max 1 $
    attackerStats ^. dsAttack
      + (attackerStats ^. dsHit)
      + (preparedAttackMove preparedAttack ^. attackMoveDamage) * 4

dodgePower :: DerivedStats -> Int
dodgePower defenderStats =
  max 0 $ defenderStats ^. dsDodge

parryPower :: DerivedStats -> Int
parryPower defenderStats =
  max 0 $ defenderStats ^. dsParry

computeDamage :: DerivedStats -> DerivedStats -> PreparedAttack -> Int
computeDamage attackerStats defenderStats preparedAttack =
  max 1 $
    baseDamage
      + artBonus
      + attackerStats ^. dsDamageBonus
      - defenderStats ^. dsDamageReduction
  where
    baseDamage = preparedAttackMove preparedAttack ^. attackMoveDamage
    artBonus = (preparedAttackArt preparedAttack ^. artLevel) `div` 3

applyCombatHooks :: Character -> Character -> PreparedAttack -> Int -> Int
applyCombatHooks _ _ _ =
  max 1

canUseActiveSkill :: ActiveSkill -> Lens' Battle BattleState -> Combat Bool
canUseActiveSkill activeSkill state = do
  apValue <- use $ state . battleAp
  qi <- use $ state . battleQi
  cds <- use $ state . battleActiveSkillCooldowns
  effects <- use $ state . battleEffects

  let hasEnoughAp = apValue >= activeSkill ^. activeSkillApReq
      hasEnoughQi = qi >= activeSkill ^. activeSkillCost
      isOffCooldown = not $ M.member (activeSkill ^. activeSkillId) cds
      hasRequiredEffects = all (`M.member` effects) (activeSkill ^. activeSkillReqStatus)

  return $ hasEnoughAp && hasEnoughQi && isOffCooldown && hasRequiredEffects

useActiveSkill :: MartialArt -> ActiveSkill -> Lens' Battle BattleState -> Lens' Battle BattleState -> Combat ()
useActiveSkill martialArt activeSkill caster target = do
  damageAmount <-
    case activeSkill ^. activeSkillTarget of
      Self -> activeSkillDamageAmount activeSkill caster caster
      _ -> activeSkillDamageAmount activeSkill caster target
  healAmount <-
    case activeSkill ^. activeSkillTarget of
      Self -> activeSkillHealAmount activeSkill caster caster
      _ -> activeSkillHealAmount activeSkill caster target

  -- Consume AP
  caster . battleAp %= max 0 . subtract (activeSkill ^. activeSkillApReq)

  -- Consume Qi
  caster . battleQi -= activeSkill ^. activeSkillCost

  -- Set cooldown
  caster . battleActiveSkillCooldowns . at (activeSkill ^. activeSkillId) .= Just (activeSkill ^. activeSkillCooldown)

  -- Apply effects based on target type
  case activeSkill ^. activeSkillTarget of
    Single -> applyActiveSkillEffects activeSkill caster target damageAmount healAmount
    Self   -> applyActiveSkillEffects activeSkill caster caster damageAmount healAmount
    All    -> applyActiveSkillEffects activeSkill caster target damageAmount healAmount  -- For now, same as Single

  uid <- use battleOwner
  casterName <- use $ caster . battleChar . charName
  targetName <-
    case activeSkill ^. activeSkillTarget of
      Self -> use $ caster . battleChar . charName
      _ -> use $ target . battleChar . charName
  visual <- resolveAnimationRef martialArt (activeSkill ^. activeSkillAnimation)
  tell
    [ ( uid,
        combatEventResp
          CombatEventActiveSkill
          casterName
          targetName
          (CombatScriptText $ activeSkill ^. activeSkillMsg)
          damageAmount
          healAmount
          activeSkillResult
          visual
      )
    ]
  where
    activeSkillResult =
      case activeSkill ^. activeSkillDamage of
        Just _ -> CombatHit
        Nothing -> CombatEffect

activeSkillDamageAmount :: ActiveSkill -> Lens' Battle BattleState -> Lens' Battle BattleState -> Combat (Maybe Int)
activeSkillDamageAmount activeSkill caster target =
  case activeSkill ^. activeSkillDamage of
    Nothing -> pure Nothing
    Just baseDamage -> do
      casterStats <- battleDerivedStats caster
      targetStats <- battleDerivedStats target
      casterChar <- use $ caster . battleChar
      let artBonus = activeSkillArtLevel activeSkill casterChar `div` 2
          mitigation = (targetStats ^. dsDefense) `div` 8
      pure . Just . max 1 $ baseDamage + artBonus + (casterStats ^. dsDamageBonus) - mitigation

activeSkillHealAmount :: ActiveSkill -> Lens' Battle BattleState -> Lens' Battle BattleState -> Combat (Maybe Int)
activeSkillHealAmount activeSkill caster _target =
  case activeSkill ^. activeSkillHeal of
    Nothing -> pure Nothing
    Just baseHeal -> do
      casterStats <- battleDerivedStats caster
      pure . Just $ baseHeal + max 0 ((casterStats ^. dsVitality) - 10) `div` 4

activeSkillArtLevel :: ActiveSkill -> Character -> Int
activeSkillArtLevel activeSkill char =
  case requiredLevels <> preparedLevels of
    [] -> 0
    levels -> maximum levels
  where
    requiredLevels = map (`characterKnownArtLevel` char) (activeSkill ^. activeSkillReqArts)
    preparedLevels = map (^. artLevel) . M.elems $ char ^. charPrepare

applyActiveSkillEffects :: ActiveSkill -> Lens' Battle BattleState -> Lens' Battle BattleState -> Maybe Int -> Maybe Int -> Combat ()
applyActiveSkillEffects activeSkill caster target damageAmount healAmount = do
  case damageAmount of
    Just dmg -> do
      target . battleChar . charHP -= dmg
    Nothing -> return ()

  case healAmount of
    Just heal -> do
      maxHp <- (^. dsMaxHp) <$> battleDerivedStats target
      target . battleChar . charHP %= min maxHp . (+ heal)
    Nothing -> return ()

  forM_ (activeSkill ^. activeSkillEffTarget) $ \(effId, duration, value) -> do
    let activeEff = ActiveEffect
          { _activeEffectDef = effId,
            _activeEffectRemaining = duration,
            _activeEffectValue = value
          }
    target . battleEffects . at effId .= Just activeEff

  forM_ (activeSkill ^. activeSkillEffSelf) $ \(effId, duration, value) -> do
    let activeEff = ActiveEffect
          { _activeEffectDef = effId,
            _activeEffectRemaining = duration,
            _activeEffectValue = value
          }
    caster . battleEffects . at effId .= Just activeEff
