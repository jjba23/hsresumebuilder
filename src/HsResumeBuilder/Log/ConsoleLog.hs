{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsResumeBuilder.Log.ConsoleLog where

import Free.AlaCarte
import HsResumeBuilder.Log
import RIO hiding (LogLevel, log, logDebug, logError, logInfo, (^.))
import RIO.ByteString.Lazy qualified as BL
import RIO.Text qualified as T
import RIO.Time

instance Exec Logger where
  execAlgebra = \case
    LogInfo m f -> doLog Info m f
    LogError m f -> doLog Error m f
    LogDebug m f -> doLog Debug m f

doLog :: (MonadIO m) => LogLevel -> Text -> m b -> m b
doLog level m f = do
  now <- iso8601
  let logExpr = now <> " " <> showLevel level <> " " <> m
   in BL.putStrLn (BL.fromStrict . T.encodeUtf8 $ logExpr) >> f

data LogLevel = Info | Error | Debug deriving (Eq)

instance Show LogLevel where
  show Info = "[INFO]"
  show Error = "[ERROR]"
  show Debug = "[DEBUG]"

showLevel :: LogLevel -> Text
showLevel = T.pack . show

-- Construct format string according to <http://en.wikipedia.org/wiki/ISO_8601 ISO-8601>.
iso8601 :: (MonadIO m) => m Text
iso8601 = do
  n <- getCurrentTime
  let n' = T.pack $ formatTime defaultTimeLocale (T.unpack "%Y-%m-%dT%H:%M:%SZ") n
  pure $ "[" <> n' <> "]"
