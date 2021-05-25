module LanguageServer.IdePurescript.Clean where

import Prelude

import Data.Array (find, fold)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.Traversable (traverse)
import Effect.Aff (Aff, attempt, message)
import LanguageServer.IdePurescript.Config (effectiveOutputDirectory)
import LanguageServer.Types (Settings)
import Node.FS.Aff as FS
import Node.FS.Stats (isDirectory)
import Node.Path as Path

clean :: Settings -> Aff (Either String String)
clean settings = do
  let
    outputDir = effectiveOutputDirectory settings
  attempedStats <- attempt $ FS.stat outputDir
  case attempedStats of
    Left err -> pure $ Left $ "Could not find directory to clean. " <> message err
    Right stats -> case isDirectory stats of
      false -> pure $ Left $ "Target \"" <> outputDir <> "\" is not a directory"
      true -> do
        msg <- (processDir false) outputDir
        case length msg of
          0 -> pure $ Right $ "Nothing to clean in directory \"" <> outputDir <> "\""
          _ -> pure $ Right $ msg <> "Successfully cleaned directory \"" <> outputDir <> "\""

type Processor = Boolean -> Path.FilePath -> Aff String
processDir :: Processor
processDir markedForRemoval path = do
  stats <- FS.stat path
  case isDirectory stats of
    false -> do
      case markedForRemoval of
        true -> removeFile path
        false -> do
          case maybeRemovableFile path of
            Nothing -> pure ""
            _ -> removeFile path
    true -> do
      contentPartialPaths <- FS.readdir path
      let
        contentFullPaths :: Array Path.FilePath
        contentFullPaths = map (joinPaths path) contentPartialPaths
        removeDir :: Path.FilePath -> Array Path.FilePath -> Aff String
        removeDir = removeDirectory processDir
      case markedForRemoval of
        true -> removeDir path contentFullPaths
        false -> case maybeRemovableContents contentPartialPaths of
          Nothing -> do
            msgs <- traverse (processDir false) contentFullPaths
            pure $ fold msgs
          _ -> removeDir path contentFullPaths

joinPaths :: Path.FilePath -> Path.FilePath -> Path.FilePath
joinPaths parent child = Path.concat [ parent, child ]

removeFile :: Path.FilePath -> Aff String
removeFile filePath = do
  FS.unlink filePath
  pure ("File \"" <> filePath <> "\" was removed" <> "\n")

filesToRemove :: Array Path.FilePath
filesToRemove = ["cache-db.json"]

maybeRemovableFile :: Path.FilePath -> Maybe Path.FilePath
maybeRemovableFile filePath = find (\ x -> x == (Path.basename filePath)) filesToRemove

directoryRemovalMarker :: Path.FilePath
directoryRemovalMarker = "externs.cbor"

maybeRemovableContents :: Array Path.FilePath -> Maybe (Array Path.FilePath)
maybeRemovableContents dirContents = case find (\ x -> x == directoryRemovalMarker) dirContents of
  Nothing -> Nothing
  _ -> Just dirContents

removeDirectory :: Processor -> Path.FilePath -> Array Path.FilePath -> Aff String
removeDirectory processor dirPath contentFullPaths = do
  let
    dirRemovalMsg :: String
    dirRemovalMsg = "Directory \"" <> dirPath <> "\" was removed" <> "\n"
  _ <- traverse (processor true) contentFullPaths
  FS.rmdir dirPath
  pure dirRemovalMsg