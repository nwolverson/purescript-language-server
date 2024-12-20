module IdePurescript.Exec where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Node.ChildProcess.Types (Shell, enableShell)
import Node.Path as Path
import Node.Platform (Platform(..))
import Node.Process (getEnv, lookupEnv, platform)
import Node.Which (which')
import PscIde.Server (Executable(..), findBins')

shellSetting :: Maybe Shell
shellSetting = case platform of
  Just Win32 -> Just enableShell
  _ -> Nothing

findBins :: forall a. Either a String -> String -> Aff (Array Executable)
findBins pathVar server = do
  env <- liftEffect getEnv
  findBins'
    { pathExt: Nothing
    , path: either (const Nothing) Just pathVar
    , env: either (const Nothing) (Just <<< flip (Object.insert "PATH") env)
        pathVar
    , shell: shellSetting
    }
    server

findBinsNoVersion ::
  forall a.
  { path :: Maybe String, pathExt :: Maybe String | a } ->
  String ->
  Aff (Array Executable)
findBinsNoVersion { path, pathExt } executable = do
  bins <- which' { path, pathExt } executable <|> pure []
  pure $ (\bin -> Executable bin Nothing) <$> bins

getPathVar :: Boolean -> String -> Effect (Either String String)
getPathVar addNpmBin rootDir = do
  processPath <- lookupEnv "PATH"
  pure
    $
      if addNpmBin then
        Right $ addNpmBinPath rootDir processPath
      else
        Left $ fromMaybe "" processPath

addNpmBinPath :: String -> Maybe String -> String
addNpmBinPath rootDir path =
  Path.concat [ rootDir, "node_modules", ".bin" ] <>
    (maybe "" (Path.delimiter <> _) path)

foreign import whichSyncImpl ::
  { path :: Nullable String, pathExt :: Nullable String } ->
  String ->
  Effect (Array String)

whichSync ::
  { path :: Maybe String, pathExt :: Maybe String } ->
  String ->
  Effect (Array String)
whichSync { path, pathExt } = whichSyncImpl
  { path: toNullable path, pathExt: toNullable pathExt }
