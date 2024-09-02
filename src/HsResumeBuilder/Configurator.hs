module HsResumeBuilder.Configurator
  ( readConfig,
    Configurator (..),
  )
where

import Data.Kind (Type)
import Free.AlaCarte
import HsResumeBuilder.Model (HsResumeBuilderConfig)
import RIO hiding (LogLevel, log, logDebug, logError, logInfo, (^.))

type Configurator :: Type -> Type
data Configurator a
  = ReadConfig FilePath (Either Text HsResumeBuilderConfig -> a)
  deriving (Functor)

readConfig :: (Configurator :<: f) => FilePath -> Free f (Either Text HsResumeBuilderConfig)
readConfig filePath = injectFree (ReadConfig filePath Pure)
