module LanguageServer.IdePurescript.Clean where

import Prelude

import Data.Array (foldM)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import LanguageServer.IdePurescript.Config (effectiveOutputDirectory)
import LanguageServer.Types (Settings)
import Node.FS.Aff as FS
import Node.FS.Stats (isDirectory)

clean :: Settings -> Aff (Either String String)
clean settings = do
  let outputDir = effectiveOutputDirectory settings
  stats <- FS.stat outputDir
  case isDirectory stats of
    false -> pure $ Left "Could not find output directory"
    true -> do
      allFiles <- FS.readdir outputDir
      let allPaths = map (append outputDir) allFiles
      foldM (\_ b -> FS.unlink b) (mempty) allPaths
      FS.rmdir outputDir
      pure $ Right $ "Successfully removed directory \"" <> outputDir <> "\""