{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsResumeBuilder.Renderer.HtmlRenderer () where

import Free.AlaCarte
import HsResumeBuilder.Renderer
import HsResumeBuilder.Themes.JJBA.Template qualified as JJBA
import RIO hiding (LogLevel, log, logDebug, logError, logInfo, (^.))
import RIO.ByteString.Lazy qualified as BL
import RIO.Text qualified as T
import Text.Blaze.Html.Renderer.String (renderHtml)

instance Exec Renderer where
  execAlgebra (RenderResume cfg filePath next) = do
    let generatedContent =
          T.pack . renderHtml . JJBA.renderResume $ cfg
    outputFileHandle <- openFile filePath ReadWriteMode
    _ <- liftIO $ BL.hGetContents outputFileHandle
    hClose outputFileHandle
    liftIO $ BL.writeFile filePath (fromString . T.unpack $ generatedContent)
    next
