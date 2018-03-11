module IdePurescript.Exec where

import Prelude
import Node.Path as Path
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (either, Either(..))
import Data.Maybe (fromMaybe, maybe, Maybe(..))
import Data.StrMap (insert)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.Process (getEnv, PROCESS, lookupEnv)
import PscIde.Server (findBins', Executable)

findBins :: forall a eff. Either a String -> String -> Aff (fs :: FS, buffer :: BUFFER, cp :: CHILD_PROCESS, process :: PROCESS | eff) (Array Executable)
findBins pathVar server = do
  env <- liftEff getEnv
  findBins'
    { pathExt: Nothing
    , path: either (const Nothing) Just pathVar
    , env: either (const Nothing) (Just <<< flip (insert "PATH") env) pathVar
    }
    server

getPathVar :: forall eff. Boolean -> String -> Eff (process :: PROCESS | eff) (Either String String)
getPathVar addNpmBin rootDir = do
  processPath <- lookupEnv "PATH"
  pure $ if addNpmBin
    then Right $ addNpmBinPath rootDir processPath
    else Left $ fromMaybe "" processPath

addNpmBinPath :: String -> Maybe String -> String
addNpmBinPath rootDir path =
  Path.concat [ rootDir, "node_modules", ".bin" ] <> (maybe "" (Path.delimiter <> _) path)
