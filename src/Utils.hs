{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Utils where

import Control.Monad (liftM)
import qualified Control.Monad
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError))
import Control.Monad.State.Strict (StateT, mapStateT, MonadState)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text, pack)
import Data.Yaml (FromJSON, decodeFileEither)
import Relude (toText)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Control.Monad.Random (randomRIO, MonadRandom, Random (randomR), getRandomR)
import Control.Lens

-- Take a processing function, load and parse all yaml files in a path, and apply the function to the parsed yaml
loadYamlProperties :: (FromJSON a, Ord k) => (a -> (k, v)) -> FilePath -> IO (Either Text (M.Map k v))
loadYamlProperties f dir = do
  isDir <- doesDirectoryExist dir
  if isDir
    then do
      files <- listDirectory dir
      let yamlFiles = filter ((== ".yaml") . takeExtension) files
      items <- Control.Monad.forM yamlFiles $ \file -> do
        let path = dir </> file
        result <- decodeFileEither path
        return $ case result of
          Left err -> Nothing
          Right item -> Just $ f item
      return $ Right $ M.fromList $ catMaybes items
    else return $ Left $ "Directory does not exist: " <> toText dir

liftMaybeT :: Monad m => e -> StateT s (MaybeT m) a -> StateT s (ExceptT e m) a
liftMaybeT errMsg = mapStateT $ \s -> ExceptT $ maybe (Left errMsg) Right <$> runMaybeT s


randomSelect :: MonadRandom m => [a] -> m a
randomSelect xs = do
  index <- getRandomR (0, length xs - 1)
  return $ xs !! index


getsL :: (At m, MonadError e m1, MonadState s m1) => Lens' s m -> Index m -> e -> m1 (IxValue m)
getsL field key err = do
  mValue <- use $ field . at key
  case mValue of
    Nothing -> throwError err
    Just value -> return value
