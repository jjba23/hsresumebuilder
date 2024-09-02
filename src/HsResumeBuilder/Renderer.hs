module HsResumeBuilder.Renderer
  ( Renderer (..),
    renderResume,
  )
where

import Data.Kind (Type)
import Free.AlaCarte
import HsResumeBuilder.Model
  ( HsResumeBuilderConfig,
  )
import RIO hiding (LogLevel, log, logDebug, logError, logInfo, (^.))

type Renderer :: Type -> Type
data Renderer a
  = RenderResume HsResumeBuilderConfig FilePath a
  deriving (Functor)

renderResume :: (Renderer :<: f) => HsResumeBuilderConfig -> FilePath -> Free f ()
renderResume cfg filePath = injectFree (RenderResume cfg filePath (Pure ()))
