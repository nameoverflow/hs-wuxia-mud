{-# LANGUAGE OverloadedStrings #-}

module Logging
  ( LogLevel (..),
    logDebug,
    logInfo,
    logError,
    logWithLevel,
  )
where

import Control.Monad (when)
import Data.Char (toUpper)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import GHC.Stack (CallStack, HasCallStack, SrcLoc (..), callStack, getCallStack)
import System.Environment (lookupEnv)
import System.IO (Handle, stderr, stdout)

data LogLevel
  = LevelDebug
  | LevelInfo
  | LevelError
  deriving (Eq, Ord, Show)

logDebug :: HasCallStack => Text -> IO ()
logDebug = logWithLevel LevelDebug

logInfo :: HasCallStack => Text -> IO ()
logInfo = logWithLevel LevelInfo

logError :: HasCallStack => Text -> IO ()
logError = logWithLevel LevelError

logWithLevel :: HasCallStack => LogLevel -> Text -> IO ()
logWithLevel level message = do
  configuredLevel <- configuredLogLevel
  when (levelPriority level >= levelPriority configuredLevel) $ do
    timestamp <- currentTimestamp
    let line =
          T.unwords
            [ timestamp,
              "[" <> levelLabel level <> "]",
              sourceLocation callStack,
              message
            ]
    TIO.hPutStrLn (logHandle level) line

configuredLogLevel :: IO LogLevel
configuredLogLevel = parseLogLevel <$> lookupEnv "MUD_LOG_LEVEL"

parseLogLevel :: Maybe String -> LogLevel
parseLogLevel level =
  case fmap (map toUpper) level of
    Just "DEBUG" -> LevelDebug
    Just "ERROR" -> LevelError
    Just "INFO" -> LevelInfo
    _ -> LevelInfo

currentTimestamp :: IO Text
currentTimestamp = do
  now <- getZonedTime
  pure $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" now

sourceLocation :: CallStack -> Text
sourceLocation stack =
  case find ((/= "Logging") . srcLocModule . snd) (getCallStack stack) of
    Just (_, loc) -> renderSourceLocation loc
    Nothing -> "unknown"

renderSourceLocation :: SrcLoc -> Text
renderSourceLocation loc =
  T.pack (srcLocFile loc)
    <> ":"
    <> T.pack (show $ srcLocStartLine loc)
    <> ":"
    <> T.pack (show $ srcLocStartCol loc)

levelLabel :: LogLevel -> Text
levelLabel LevelDebug = "DEBUG"
levelLabel LevelInfo = "INFO"
levelLabel LevelError = "ERROR"

levelPriority :: LogLevel -> Int
levelPriority LevelDebug = 0
levelPriority LevelInfo = 1
levelPriority LevelError = 2

logHandle :: LogLevel -> Handle
logHandle LevelError = stderr
logHandle _ = stdout
