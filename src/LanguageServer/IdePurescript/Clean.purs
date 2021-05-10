module LanguageServer.IdePurescript.Clean where

import Prelude
import Data.Array (foldMap)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripPrefix, stripSuffix)
import Effect.Aff (Aff, attempt, message)
import LanguageServer.IdePurescript.Config (effectiveOutputDirectory)
import LanguageServer.Types (Settings)
import Node.FS.Aff as FS
import Node.FS.Stats (isDirectory)

clean :: Settings -> Aff (Either String String)
clean settings = do
  let
    outputDir = effectiveOutputDirectory settings
  attempedStats <- attempt $ FS.stat outputDir
  case attempedStats of
    Left err -> pure $ Left $ "Could not find directory to clean:\n" <> message err
    Right stats -> case isDirectory stats of
      false -> pure $ Left $ "Target \"" <> outputDir <> "\" is not a directory"
      true -> do
        removeDir outputDir
        pure $ Right $ "Successfully removed directory \"" <> outputDir <> "\""

removeDir :: String -> Aff Unit
removeDir path = do
  stats' <- FS.stat path
  case isDirectory stats' of
    false -> FS.unlink path
    true -> do
      allFiles <- FS.readdir path
      let
        allPaths = map (joinPaths path) allFiles
      foldMap removeDir allPaths
      FS.rmdir path

joinPaths :: String -> String -> String
joinPaths parent child = strippedParent <> "/" <> strippedChild
  where
  strippedParent = fromMaybe parent (stripSuffix (Pattern "/") parent)

  strippedChild = fromMaybe child (stripPrefix (Pattern "/") child)
