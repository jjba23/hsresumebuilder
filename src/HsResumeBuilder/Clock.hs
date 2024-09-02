module HsResumeBuilder.Clock
  ( timeElapsedUntilNow,
    now,
    Clock (..),
  )
where

import Data.Kind (Type)
import Free.AlaCarte
import RIO hiding (LogLevel, log, logDebug, logError, logInfo, (^.))
import RIO.Time

type Clock :: Type -> Type
data Clock a
  = TimeElapsedUntilNow UTCTime (Text -> a)
  | Now (UTCTime -> a)
  deriving (Functor)

timeElapsedUntilNow :: (Clock :<: f) => UTCTime -> Free f Text
timeElapsedUntilNow fromTime = injectFree (TimeElapsedUntilNow fromTime Pure)

now :: (Clock :<: f) => Free f UTCTime
now = injectFree (Now Pure)
