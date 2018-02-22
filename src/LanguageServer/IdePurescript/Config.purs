module LanguageServer.IdePurescript.Config where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (F, Foreign, readArray, readBoolean, readInt, readString)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import PscIde.Server (LogLevel(..))

getConfigMaybe :: forall a. (Foreign -> F a) -> String -> Foreign -> Maybe a
getConfigMaybe readValue key settings = do
    either (const Nothing) Just $ runExcept val
    where
        val = do
            ps <- settings ! "purescript"
            res <- ps ! key
            readValue res

getConfig :: forall a. (Foreign -> F a) -> String -> a -> Foreign -> a
getConfig readValue key default settings =
    fromMaybe default $ getConfigMaybe readValue key settings

getBoolean :: String -> Boolean -> Foreign -> Boolean
getBoolean = getConfig readBoolean

getString :: String -> String -> Foreign -> String
getString = getConfig readString

getInt :: String -> Int -> Foreign -> Int
getInt = getConfig readInt

type ConfigFn a = Foreign -> a

serverExe :: ConfigFn String
serverExe = getString "pscIdeServerExe" "psc-ide-server"

pursExe :: ConfigFn String
pursExe = getString "pursExe" "purs"

pscIdePort :: ConfigFn Int
pscIdePort = getInt "pscIdePort" 4242

autoCompleteAllModules :: ConfigFn Boolean
autoCompleteAllModules = getBoolean "autocompleteAllModules" true

buildCommand :: ConfigFn String
buildCommand = getString "buildCommand" "pulp build -- --json-errors"

addNpmPath :: ConfigFn Boolean
addNpmPath = getBoolean "addNpmPath" false

usePurs :: ConfigFn Boolean
usePurs = getBoolean "useCombinedExe" true

packagePath :: ConfigFn String
packagePath = getString "packagePath" "bower_components"

srcPath :: ConfigFn String
srcPath = getString "sourcePath" "src"

sourceGlobs :: ConfigFn (Array String)
sourceGlobs = getConfig (readArray >=> traverse readString) "sourceGlobs" []

censorCodes :: ConfigFn (Array String)
censorCodes = getConfig (readArray >=> traverse readString) "censorWarnings" []

autoStartPscIde :: ConfigFn Boolean
autoStartPscIde = getBoolean "autoStartPscIde" true

autocompleteAddImport :: ConfigFn Boolean
autocompleteAddImport = getBoolean "autocompleteAddImport" true

autocompleteGrouped :: ConfigFn Boolean
autocompleteGrouped = getBoolean "autocompleteGrouped" false

autocompleteLimit :: ConfigFn (Maybe Int)
autocompleteLimit = getConfigMaybe readInt "autocompleteLimit"

importsPreferredModules :: ConfigFn (Array String)
importsPreferredModules = getConfig (readArray >=> traverse readString) "importsPreferredModules" []

preludeModule :: ConfigFn String
preludeModule = getString "preludeModule" "Prelude"

fastRebuild :: ConfigFn Boolean
fastRebuild = getBoolean "fastRebuild" true

editorMode :: ConfigFn Boolean
editorMode = getBoolean "editorMode" false

polling :: ConfigFn Boolean
polling = getBoolean "polling" false

addPscPackageSources :: ConfigFn Boolean
addPscPackageSources = getBoolean "addPscPackageSources" false

logLevel :: ConfigFn (Maybe LogLevel)
logLevel = getString "pscIdelogLevel" "" >>> case _ of
    "all" -> Just All
    "none" -> Just None
    "debug" -> Just Debug
    "perf" -> Just Perf
    _ -> Nothing
