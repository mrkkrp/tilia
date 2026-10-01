{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- | Definitions to support editor integrations.
module Tilia.Editor
  ( editorSession,
    formatBuffer,
  )
where

import Data.Choice (Choice)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Tilia.Cabal.Project (ProjectRoot (..), findProjectRoot)
import Tilia.Cabal.Target
  ( Target (Everything),
    componentInPlan,
    componentsOfTarget,
  )
import Tilia.Format
  ( FormatError,
    PlanSource (..),
    Session,
    newSession,
    sessionRoot,
  )
import Tilia.Ignore (isIgnoredInProject)
import Tilia.Run (Outcome (Unchanged), outcomeOf)

-- | Create a 'Session' for the project a file belongs to, with a plan that
-- covers every component of it.
editorSession ::
  -- | The file.
  FilePath ->
  -- | A build plan to trust as up to date rather than have Cabal solve one.
  Maybe FilePath ->
  -- | Whether to read from and write to the cache.
  Choice "useCache" ->
  -- | Whether to download sources that are missing.
  Choice "download" ->
  -- | Check AST equivalence.
  Choice "checkAst" ->
  -- | Check idempotence.
  Choice "checkIdempotence" ->
  -- | Record how fixities were settled, to be read with
  -- 'Tilia.Format.fixityNotesOf'.
  Choice "debugFixity" ->
  IO (Either FormatError Session)
editorSession
  file
  givenPlan
  caching
  downloading
  checkAst
  checkIdempotence
  debugFixity = do
    planSource <- case givenPlan of
      Just plan -> pure (GivenPlan plan)
      Nothing -> PlanFromCabal <$> components
    newSession
      file
      planSource
      caching
      downloading
      checkAst
      checkIdempotence
      debugFixity
    where
      components =
        findProjectRoot file >>= \case
          Nothing -> pure []
          Just root ->
            either (const []) (mapMaybe componentInPlan)
              <$> componentsOfTarget root Everything

-- | Format given text on the assumption it comes from the specified
-- 'FilePath'.
formatBuffer ::
  -- | The session.
  Session ->
  -- | The file.
  FilePath ->
  -- | The text to format.
  Text ->
  IO Outcome
formatBuffer session file before =
  isIgnoredInProject (prPath (sessionRoot session)) file >>= \case
    True -> pure Unchanged
    False -> outcomeOf session file before
