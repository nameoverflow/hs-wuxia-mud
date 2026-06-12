{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Utils where

import Control.Monad (liftM)
import qualified Control.Monad
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError))
import Control.Monad.State.Strict (StateT, mapStateT, MonadState)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Aeson (FromJSON, Result (..), Value, fromJSON)
import Data.Either (partitionEithers)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither)
import Relude (toText)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Control.Monad.Random (MonadRandom, getRandomR)
import Control.Lens
import qualified Data.Text.IO as TIO

class FromJSON a => Configurable a where
  loadConfigFrom :: FilePath -> IO (Either Text a)
  loadConfigFrom p = do
    result <- decodeFileEither p
    return $ case result of
      Right a -> Right a
      Left e -> Left . pack $ show e
  
  loadConfigFromDir :: Ord k => (a -> k) -> FilePath -> IO (Either Text (M.Map k a))
  loadConfigFromDir f dir = do
    isDir <- doesDirectoryExist dir
    if isDir
      then do
        files <- listDirectory dir
        let yamlFiles = Prelude.filter ((== ".yaml") . takeExtension) files
        items <- Control.Monad.forM yamlFiles $ \file -> do
          let path = dir </> file
          result <- decodeFileEither path
          case result of
            Left err ->
              pure $ Left $ toText path <> ": " <> toText (show err)
            Right (value :: Value) ->
              case fromJSON value of
                Success item -> pure $ Right [(f item, item)]
                Error singleErr ->
                  case fromJSON value of
                    Success many -> pure $ Right $ map (\item -> (f item, item)) many
                    Error listErr ->
                      pure $
                        Left $
                          toText path
                            <> ": "
                            <> toText singleErr
                            <> "; "
                            <> toText listErr
        let (errs, loadedItems) = partitionEithers items
        case errs of
          [] -> do
            let ret = M.fromList $ concat loadedItems
            TIO.putStrLn $ "Loaded " <> toText (show (M.size ret)) <> " items from " <> toText dir
            return $ Right ret
          _ ->
            return $ Left $ "Failed loading " <> toText dir <> ":\n" <> T.unlines errs
      else return $ Left $ "Directory does not exist: " <> toText dir


liftMaybeT :: Monad m => e -> StateT s (MaybeT m) a -> StateT s (ExceptT e m) a
liftMaybeT errMsg = mapStateT $ \s -> ExceptT $ maybe (Left errMsg) Right <$> runMaybeT s


randomSelect :: MonadRandom m => [a] -> m (Maybe a)
randomSelect [] = return Nothing
randomSelect xs = do
  idx <- getRandomR (0, length xs - 1)
  return $ xs ^? element idx


getsL :: (At m, MonadError e m1, MonadState s m1) => Lens' s m -> Index m -> e -> m1 (IxValue m)
getsL field key err = do
  mValue <- use $ field . at key
  case mValue of
    Nothing -> throwError err
    Just value -> return value
