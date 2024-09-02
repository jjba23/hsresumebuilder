{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsResumeBuilder.Configurator.TomlConfigurator () where

import Free.AlaCarte
import HsResumeBuilder.Configurator
import HsResumeBuilder.Model
import RIO hiding (LogLevel, log, logDebug, logError, logInfo, (^.))
import Toml

instance Exec Configurator where
  execAlgebra (ReadConfig filePath next) = do
    parseResult <- decodeFileEither hsResumeBuilderConfigCodec filePath
    case parseResult of
      Left e -> next . Left $ prettyTomlDecodeErrors e
      Right r -> next $ Right r
