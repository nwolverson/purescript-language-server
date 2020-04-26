module IdePurescript.PscIde (getCompletion, getCompletion', cwd, loadDeps, getType, eitherToErr
  , getPursuitModuleCompletion, getPursuitCompletion, getAvailableModules, getLoadedModules, SearchResult, ModuleSearchResult
  , getTypeInfo) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Array (head, null)
import Data.Either (Either(Right, Left))
import Data.Maybe (maybe, Maybe(..))
import Data.Nullable (toNullable, Nullable)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (global)
import Effect.Aff (Aff, error)
import IdePurescript.Regex (replace')
import PscIde as P
import PscIde.Command (CompletionOptions, TypePosition)
import PscIde.Command as C

eitherToErr :: forall a. Aff (Either String a) -> (Aff a)
eitherToErr c = do
  r <- c
  case r of
    Left s -> throwError (error s)
    Right res -> pure res

result :: forall a b. (a -> b) ->  Aff (Either String a) -> Aff b
result f a = eitherToErr ((f <$> _) <$> a)

cwd :: Int -> Aff String
cwd = result runMsg <<< P.cwd

runMsg :: C.Message -> String
runMsg (C.Message m) = m

getImports' :: Int -> String -> Aff (Array { module :: String, qualifier :: Nullable String })
getImports' port s = result conv $ P.listImports port s
  where
  conv (C.ImportList { moduleName, imports }) = conv' <$> imports
  conv' (C.Import {moduleName, qualifier}) = {
    "module": moduleName,
    qualifier: toNullable qualifier
  }

getAvailableModules :: Int -> Aff (Array String)
getAvailableModules = result conv <<< P.listAvailableModules
  where
  conv (C.ModuleList modules) = primModules <> modules


getLoadedModules :: Int -> Aff (Array String)
getLoadedModules = result conv <<< P.listLoadedModules
  where
  conv (C.ModuleList modules) = primModules <> modules

primModules :: Array String
primModules = ["Prim"
              ,"Prim.Boolean"
              ,"Prim.Ordering"
              ,"Prim.Row"
              ,"Prim.RowList"
              ,"Prim.Symbol"
              ,"Prim.TypeError"
              ]

abbrevType :: String -> String
abbrevType = replace' r "$1"
  where r = regex """(?:\w+\.)+(\w+)""" $ global

type TypeResult = {type :: String, identifier :: String, module :: String, position :: Maybe TypePosition}

getTypeInfo :: Int -> String -> Maybe String -> Maybe String -> Array String -> (String -> Array String)
  -> Aff (Maybe C.TypeInfo)
getTypeInfo port text currentModule modulePrefix unqualModules getQualifiedModule =
  result head $ P.type' port text moduleFilters currentModule
  where
    moduleFilters = [ C.ModuleFilter $ maybe unqualModules getQualifiedModule modulePrefix ]

getType :: Int -> String -> Maybe String -> Maybe String -> Array String -> (String -> Array String)
  -> Aff String
getType port text currentModule modulePrefix unqualModules getQualifiedModule =
  maybe "" getType' <$> getTypeInfo port text currentModule modulePrefix unqualModules getQualifiedModule
  where
  getType' (C.TypeInfo { type' }) = type'

type CompletionResult = {type :: String, identifier :: String, module :: String}

getCompletion :: Int -> String -> Maybe String -> Maybe String -> Array String -> (String -> Array String) -> CompletionOptions
  -> Aff (Array C.TypeInfo)
getCompletion port prefix =
  getCompletion' Nothing [C.PrefixFilter prefix] port

getCompletion' :: Maybe C.Matcher -> Array C.Filter -> Int -> Maybe String -> Maybe String -> Array String -> (String -> Array String) -> CompletionOptions
  -> Aff (Array C.TypeInfo)
getCompletion' matcher mainFilter port currentModule modulePrefix unqualModules getQualifiedModule opts =
  eitherToErr $ P.complete port (mainFilter <> moduleFilters) matcher currentModule opts
  where
  modules = maybe unqualModules getQualifiedModule modulePrefix
  moduleFilters = [ C.ModuleFilter $ if null modules then unqualModules else modules ]

loadDeps :: Int -> String -> Aff String
loadDeps port main = result runMsg $ P.load port [] [main]

type SearchResult = { module :: String, package :: String, type:: Maybe String, identifier :: String, text :: String }

getPursuitCompletion :: Int -> String -> Aff (Array SearchResult)
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

getPursuitModuleCompletion :: Int -> String -> Aff (Array ModuleSearchResult)
getPursuitModuleCompletion port str = result (map convPursuitModuleCompletion) $ complete str
  where

  complete :: String -> P.Cmd (Array ModuleCompletion)
  complete q = P.sendCommand port (C.Pursuit C.Package q)

  convPursuitModuleCompletion :: ModuleCompletion -> ModuleSearchResult
  convPursuitModuleCompletion (ModuleCompletion { module', package })
    = { package, "module": module' }
