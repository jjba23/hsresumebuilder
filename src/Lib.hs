module Lib
  ( runMain,
  )
where

import Free.AlaCarte
import HsResumeBuilder.Clock
import HsResumeBuilder.Clock.LiveClock ()
import HsResumeBuilder.Configurator
import HsResumeBuilder.Configurator.TomlConfigurator ()
import HsResumeBuilder.Log
import HsResumeBuilder.Log.ConsoleLog ()
import HsResumeBuilder.Renderer
import HsResumeBuilder.Renderer.HtmlRenderer ()
import RIO hiding (LogLevel, log, logDebug, logError, logInfo, (^.))
import RIO.Text qualified as T
import RIO.Time

runMain :: IO ()
runMain = exec @(Configurator :+: Renderer :+: Logger :+: Clock) $ do
  programStartUTCTime <- now
  logInfo "Starting hsResumeBuilder!"
  logInfo "About to read configuration from TOML file!"
  maybeCfg <- readConfig "config.toml"

  case maybeCfg of
    Left e -> do
      logInfo "No valid configuration could be read!"
      logInfo e
      logTimeElapsed programStartUTCTime
    Right cfg -> do
      logDebug (T.pack . show $ cfg)
      logInfo "About to render resume to HTML file!"
      renderResume cfg "output.html"
      logInfo "Terminating hsResumeBuilder!"
      logTimeElapsed programStartUTCTime

logTimeElapsed ::
  (Clock :<: f, Logger :<: f) =>
  UTCTime ->
  Free f ()
logTimeElapsed programStartUTCTime = do
  t <- timeElapsedUntilNow programStartUTCTime
  logInfo ("Time elapsed: " <> t)
