{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsResumeBuilder.Clock.LiveClock where

import Free.AlaCarte
import HsResumeBuilder.Clock
import RIO hiding (LogLevel, log, logDebug, logError, logInfo, (^.))
import RIO.Text qualified as T
import RIO.Time

instance Exec Clock where
  execAlgebra (TimeElapsedUntilNow fromTime f) = do
    diffWithNow fromTime >>= f
  execAlgebra (Now f) = do
    getCurrentTime >>= f

diffWithNow :: (MonadIO m) => UTCTime -> m Text
diffWithNow fromTime = do
  now' <- getCurrentTime
  pure $ T.pack . show $ diffUTCTime now' fromTime
