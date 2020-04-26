module IdePurescript.Exec where

import Prelude

import Data.Either (either, Either(..))
import Data.Maybe (fromMaybe, maybe, Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Node.Path as Path
import Node.Process (getEnv, lookupEnv)
import PscIde.Server (findBins', Executable)


findBins :: forall a. Either a String -> String -> Aff (Array Executable)
findBins pathVar server = do
  env <- liftEffect getEnv
  findBins'
    { pathExt: Nothing
    , path: either (const Nothing) Just pathVar
    , env: either (const Nothing) (Just <<< flip (Object.insert "PATH") env) pathVar
    }
    server

getPathVar :: Boolean -> String -> Effect (Either String String)
getPathVar addNpmBin rootDir = do
  processPath <- lookupEnv "PATH"
  pure $ if addNpmBin
    then Right $ addNpmBinPath rootDir processPath
    else Left $ fromMaybe "" processPath

addNpmBinPath :: String -> Maybe String -> String
addNpmBinPath rootDir path =
  Path.concat [ rootDir, "node_modules", ".bin" ] <> (maybe "" (Path.delimiter <> _) path)
