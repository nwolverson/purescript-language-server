module IdePurescript.PscIde (getCompletion, getCompletion', cwd, loadDeps, getType, eitherToErr
  , getPursuitModuleCompletion, getPursuitCompletion, getAvailableModules, getLoadedModules, SearchResult, ModuleSearchResult
  , getTypeInfo) where

import Prelude
import Control.Monad.Eff.Exception as Ex
import PscIde as P
import PscIde.Command as C
import Control.Monad.Aff (Aff)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Array (head, null)
import Data.Either (Either(Right, Left))
import Data.Maybe (maybe, Maybe(..))
import Data.Nullable (toNullable, Nullable)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (global)
import IdePurescript.Regex (replace')
import PscIde.Command (CompletionOptions, TypePosition)

eitherToErr :: forall a eff. Aff (net :: P.NET | eff) (Either String a) -> (Aff (net :: P.NET | eff) a)
eitherToErr c = do
  r <- c
  case r of
    Left s -> throwError (Ex.error s)
    Right res -> pure res

result :: forall eff a b. (a -> b) ->  Aff (net :: P.NET | eff) (Either String a) -> Aff (net :: P.NET | eff) b
result f a = eitherToErr ((f <$> _) <$> a)

cwd :: forall eff. Int -> Aff (net :: P.NET | eff) String
cwd = result runMsg <<< P.cwd

runMsg :: C.Message -> String
runMsg (C.Message m) = m

getImports' :: forall eff. Int -> String
  -> Aff (net :: P.NET | eff) (Array { module :: String, qualifier :: Nullable String })
getImports' port s = result conv $ P.listImports port s
  where
  conv (C.ImportList { moduleName, imports }) = conv' <$> imports
  conv' (C.Import {moduleName, qualifier}) = {
    "module": moduleName,
    qualifier: toNullable qualifier
  }

getAvailableModules :: forall eff. Int -> Aff (net :: P.NET | eff) (Array String)
getAvailableModules = result conv <<< P.listAvailableModules
  where
  conv (C.ModuleList modules) = modules

getLoadedModules :: forall eff. Int -> Aff (net :: P.NET | eff) (Array String)
getLoadedModules = result conv <<< P.listLoadedModules
  where
  conv (C.ModuleList modules) = modules

abbrevType :: String -> String
abbrevType = replace' r "$1"
  where r = regex """(?:\w+\.)+(\w+)""" $ global

type TypeResult = {type :: String, identifier :: String, module :: String, position :: Maybe TypePosition}

getTypeInfo :: forall eff. Int -> String -> Maybe String -> Maybe String -> Array String -> (String -> Array String)
  -> Aff (net :: P.NET | eff) (Maybe C.TypeInfo)
getTypeInfo port text currentModule modulePrefix unqualModules getQualifiedModule =
  result head $ P.type' port text moduleFilters currentModule
  where
    moduleFilters = [ C.ModuleFilter $ maybe unqualModules getQualifiedModule modulePrefix ]

getType :: forall eff. Int -> String -> Maybe String -> Maybe String -> Array String -> (String -> Array String)
  -> Aff (net :: P.NET | eff) String
getType port text currentModule modulePrefix unqualModules getQualifiedModule =
  maybe "" getType' <$> getTypeInfo port text currentModule modulePrefix unqualModules getQualifiedModule
  where
  getType' (C.TypeInfo { type' }) = type'

type CompletionResult = {type :: String, identifier :: String, module :: String}

getCompletion :: forall eff. Int -> String -> Maybe String -> Maybe String -> Array String -> (String -> Array String) -> CompletionOptions
  -> Aff (net :: P.NET | eff) (Array C.TypeInfo)
getCompletion port prefix =
  getCompletion' Nothing [C.PrefixFilter prefix] port

getCompletion' :: forall eff. Maybe C.Matcher -> Array C.Filter -> Int -> Maybe String -> Maybe String -> Array String -> (String -> Array String) -> CompletionOptions
  -> Aff (net :: P.NET | eff) (Array C.TypeInfo)
getCompletion' matcher mainFilter port currentModule modulePrefix unqualModules getQualifiedModule opts =
  eitherToErr $ P.complete port (mainFilter <> moduleFilters) matcher currentModule opts
  where
  modules = maybe unqualModules getQualifiedModule modulePrefix
  moduleFilters = [ C.ModuleFilter $ if null modules then unqualModules else modules ]

loadDeps :: forall eff. Int -> String
  -> Aff (net :: P.NET | eff) String
loadDeps port main = result runMsg $ P.load port [] [main]

type SearchResult = { module :: String, package :: String, type:: Maybe String, identifier :: String, text :: String }

getPursuitCompletion :: forall eff. Int -> String -> Aff (net :: P.NET | eff) (Array SearchResult)
getPursuitCompletion port str = result (map convPursuitCompletion) $ P.pursuitCompletion port str

convPursuitCompletion :: C.PursuitCompletion -> SearchResult
convPursuitCompletion (C.PursuitCompletion { identifier, type', module', package, text })
  = { identifier, package, type: type', "module": module', text }

data ModuleCompletion = ModuleCompletion {
  module' :: String,
  package :: String
}

instance decodeModuleCompletion :: DecodeJson ModuleCompletion where
  decodeJson json = do
    o <- decodeJson json
    module' <- o .? "module"
    package <- o .? "package"
    pure (ModuleCompletion {
      module': module',
      package: package
      })

type ModuleSearchResult = { module :: String, package :: String }

getPursuitModuleCompletion :: forall eff. Int -> String
  -> Aff (net :: P.NET | eff) (Array ModuleSearchResult)
getPursuitModuleCompletion port str = result (map convPursuitModuleCompletion) $ complete str
  where

  complete :: String -> P.Cmd (Array ModuleCompletion)
  complete q = P.sendCommand port (C.Pursuit C.Package q)

  convPursuitModuleCompletion :: ModuleCompletion -> ModuleSearchResult
  convPursuitModuleCompletion (ModuleCompletion { module', package })
    = { package, "module": module' }
