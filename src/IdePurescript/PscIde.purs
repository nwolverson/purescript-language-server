module IdePurescript.PscIde
  ( getCompletion
  , getCompletion'
  , typesInModule
  , cwd
  , loadDeps
  , getType
  , eitherToErr
  , getPursuitModuleCompletion
  , getPursuitCompletion
  , getAvailableModules
  , getLoadedModules
  , SearchResult
  , ModuleSearchResult
  , getTypeInfo
  , getModuleInfo
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Array (head, null)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff, error)
import PscIde as P
import PscIde.Command (CompletionOptions, DeclarationType(..), TypePosition)
import PscIde.Command as C

eitherToErr :: forall a. Aff (Either String a) -> (Aff a)
eitherToErr c = do
  r <- c
  case r of
    Left s -> throwError (error s)
    Right res -> pure res

result :: forall a b. (a -> b) -> Aff (Either String a) -> Aff b
result f a = eitherToErr ((f <$> _) <$> a)

cwd :: Int -> Aff String
cwd = result runMsg <<< P.cwd

runMsg :: C.Message -> String
runMsg (C.Message m) = m

getAvailableModules :: Int -> Aff (Array String)
getAvailableModules = result conv <<< P.listAvailableModules
  where
  conv (C.ModuleList modules) = primModules <> modules

getLoadedModules :: Int -> Aff (Array String)
getLoadedModules = result conv <<< P.listLoadedModules
  where
  conv (C.ModuleList modules) = primModules <> modules

primModules :: Array String
primModules =
  [ "Prim"
  , "Prim.Boolean"
  , "Prim.Ordering"
  , "Prim.Row"
  , "Prim.RowList"
  , "Prim.Symbol"
  , "Prim.TypeError"
  ]

type TypeResult =
  { type :: String
  , identifier :: String
  , module :: String
  , position :: Maybe TypePosition
  }

getTypeInfo ::
  Int ->
  String ->
  Maybe String ->
  Maybe String ->
  Array String ->
  (String -> Array String) ->
  Aff (Maybe C.TypeInfo)
getTypeInfo
  port
  text
  currentModule
  modulePrefix
  unqualModules
  getQualifiedModule =
  result head $ P.type' port text moduleFilters currentModule
  where
  moduleFilters =
    [ C.ModuleFilter $ maybe unqualModules getQualifiedModule modulePrefix ]

getModuleInfo :: Int -> String -> Aff (Maybe C.TypeInfo)
getModuleInfo port text =
  result head $ P.type' port text filters Nothing
  where
  filters = [ C.DeclarationFilter [ DeclModule ] ]

getType ::
  Int ->
  String ->
  Maybe String ->
  Maybe String ->
  Array String ->
  (String -> Array String) ->
  Aff String
getType port text currentModule modulePrefix unqualModules getQualifiedModule =
  maybe "" getType' <$> getTypeInfo port text currentModule modulePrefix
    unqualModules
    getQualifiedModule
  where
  getType' (C.TypeInfo { type' }) = type'

type CompletionResult =
  { type :: String, identifier :: String, module :: String }

getCompletion ::
  Int ->
  String ->
  Maybe String ->
  Maybe String ->
  Array String ->
  (String -> Array String) ->
  CompletionOptions ->
  Aff (Array C.TypeInfo)
getCompletion port prefix =
  getCompletion' Nothing [ C.PrefixFilter prefix ] port

getCompletion' ::
  Maybe C.Matcher ->
  Array C.Filter ->
  Int ->
  Maybe String ->
  Maybe String ->
  Array String ->
  (String -> Array String) ->
  CompletionOptions ->
  Aff (Array C.TypeInfo)
getCompletion'
  matcher
  mainFilter
  port
  currentModule
  modulePrefix
  unqualModules
  getQualifiedModule
  opts =
  eitherToErr $ P.complete port (mainFilter <> moduleFilters) matcher
    currentModule
    opts
  where
  modules = maybe unqualModules getQualifiedModule modulePrefix
  moduleFilters =
    [ C.ModuleFilter $ if null modules then unqualModules else modules ]

typesInModule :: Int -> String -> Aff (Array C.TypeInfo)
typesInModule port moduleName =
  eitherToErr $ P.complete port [ C.ModuleFilter [ moduleName ] ] Nothing
    (Just moduleName)
    P.defaultCompletionOptions

loadDeps :: Int -> String -> Aff String
loadDeps port main = result runMsg $ P.load port [] [ main ]

type SearchResult =
  { module :: String
  , package :: String
  , type :: Maybe String
  , identifier :: String
  , text :: String
  }

getPursuitCompletion :: Int -> String -> Aff (Array SearchResult)
getPursuitCompletion port str = result (map convPursuitCompletion) $
  P.pursuitCompletion port str

convPursuitCompletion :: C.PursuitCompletion -> SearchResult
convPursuitCompletion
  (C.PursuitCompletion { identifier, type', module', package, text }) =
  { identifier, package, type: type', "module": module', text }

data ModuleCompletion = ModuleCompletion
  { module' :: String
  , package :: String
  }

instance decodeModuleCompletion :: DecodeJson ModuleCompletion where
  decodeJson json = do
    o <- decodeJson json
    module' <- o .: "module"
    package <- o .: "package"
    pure
      ( ModuleCompletion
          { module': module'
          , package: package
          }
      )

type ModuleSearchResult = { module :: String, package :: String }

getPursuitModuleCompletion :: Int -> String -> Aff (Array ModuleSearchResult)
getPursuitModuleCompletion port str = result (map convPursuitModuleCompletion) $
  complete str
  where

  complete :: String -> P.Cmd (Array ModuleCompletion)
  complete q = P.sendCommand port (C.Pursuit C.Package q)

  convPursuitModuleCompletion :: ModuleCompletion -> ModuleSearchResult
  convPursuitModuleCompletion (ModuleCompletion { module', package }) =
    { package, "module": module' }
