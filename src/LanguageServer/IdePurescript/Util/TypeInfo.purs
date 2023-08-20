module LanguageServer.IdePurescript.Util.TypeInfo where


import Prelude

import Data.Foldable (foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import IdePurescript.Modules (getQualModule, getUnqualActiveModules)
import IdePurescript.PscIde (getTypeInfo, getTypeInfoWithImportFilter)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify, Version(..), parseVersion)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.IdePurescript.Util (maybeParseResult)
import LanguageServer.Protocol.Types (DocumentUri)
import PscIde.Command as C
import PscIde.Server (Executable(..))
import PureScript.CST.Print as CST.Print
import PureScript.CST.Range (class TokensOf) as CST
import PureScript.CST.Range (tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types (Module(..), ModuleHeader(..)) as CST

dependencyFilterAvailable :: ServerState -> Boolean
dependencyFilterAvailable (ServerState { purs }) = case purs of
  Just (Executable _bin (Just vStr)) | Just version <- parseVersion vStr ->
    version >= Version 0 15 7 Nothing
  _ -> false

getTypeInfoMaybeNew :: Notify -> ServerState -> DocumentUri ->  String -> Maybe String -> Aff (Maybe C.TypeInfo)
getTypeInfoMaybeNew notify state@(ServerState { parsedModules, modules, port: maybePort, purs }) uri text qualifier = do
  liftEffect $ notify Info $ "getTypeInfoMaybeNew: new=" <> show (dependencyFilterAvailable state) <> " version_new=" <> maybe "?" (\(Executable exe vers) -> show ({vers, parsed: parseVersion <$> vers})) purs
  case maybePort of 
    Just port ->
      if dependencyFilterAvailable state then do
        imports <- getImports
        getTypeInfoWithImportFilter port text modules.main qualifier imports
      else
        getTypeInfo port text modules.main qualifier (getUnqualActiveModules modules $ Just text) (flip getQualModule modules)
    Nothing -> pure Nothing

  where
  getImports = 
    case Map.lookup uri parsedModules of
      Just { parsed } -> 
        pure $ maybeParseResult [] parseImports parsed
      Nothing -> do
        liftEffect $ notify Warning $ "tooltips - no parsed CST for " <> show uri
        pure []

parseImports :: forall a. CST.TokensOf a => CST.Module a -> Array String
parseImports (CST.Module { header: CST.ModuleHeader { imports } }) =
  let
    printImport imp =
      String.trim $ foldMap CST.Print.printSourceToken (TokenList.toArray (tokensOf imp))
  in
    printImport <$> imports

