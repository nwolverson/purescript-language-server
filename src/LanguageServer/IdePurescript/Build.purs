module LanguageServer.IdePurescript.Build where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (filter, mapMaybe, notElem, uncons)
import Data.Either (either)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toNullable)
import Data.StrMap (StrMap, empty, fromFoldableWith)
import Data.String (trim)
import Data.String.Regex (regex, split)
import Data.String.Regex.Flags (noFlags)
import Data.Tuple (Tuple(..))
import IdePurescript.Build (Command(..), build, rebuild)
import IdePurescript.PscErrors (PscError(..), PscResult)
import IdePurescript.PscErrors as PscErrors
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import LanguageServer.IdePurescript.Config (addNpmPath, buildCommand, censorCodes)
import LanguageServer.IdePurescript.Types (ServerState(..), MainEff)
import LanguageServer.Types (Diagnostic(Diagnostic), DocumentStore, DocumentUri, Position(Position), Range(Range), Settings)
import LanguageServer.Uri (uriToFilename)
import Node.Path (resolve)

positionToRange :: PscErrors.Position -> Range
positionToRange ({ startLine, startColumn, endLine, endColumn}) =
  Range { start: Position { line: startLine-1, character: startColumn-1 }
        , end:   Position { line: endLine-1, character: endColumn-1 } }

type DiagnosticResult = { pscErrors :: Array PscError, diagnostics :: StrMap (Array Diagnostic) }

emptyDiagnostics :: DiagnosticResult
emptyDiagnostics = { pscErrors: [], diagnostics: empty }

collectByFirst :: forall a. Array (Tuple (Maybe String) a) -> StrMap (Array a)
collectByFirst x = fromFoldableWith (<>) $ mapMaybe f x
  where
  f (Tuple (Just a) b) = Just (Tuple a [b])
  f _ = Nothing

convertDiagnostics :: String -> Settings -> PscResult -> DiagnosticResult
convertDiagnostics projectRoot settings { warnings, errors } =
  { diagnostics
  , pscErrors: errors <> warnings'
  }
  where
  diagnostics = collectByFirst allDiagnostics

  allDiagnostics = (convertDiagnostic true <$> errors) <> (convertDiagnostic false <$> warnings')
  warnings' = censorWarnings settings warnings
  dummyRange = 
      Range { start: Position { line: 1, character: 1 }
            , end:   Position { line: 1, character: 1 } }
  convertDiagnostic isError (PscError { errorCode, position, message, filename }) = Tuple 
    (resolve [ projectRoot ] <$> filename)
    (Diagnostic
      { range: maybe dummyRange positionToRange position
      , severity: toNullable $ Just $ if isError then 1 else 2 
      , code: toNullable $ Just $ errorCode
      , source: toNullable $ Just "PureScript"
      , message
      })

getDiagnostics :: forall eff. DocumentUri -> Settings -> ServerState (MainEff eff) -> Aff (MainEff eff) DiagnosticResult
getDiagnostics uri settings state = do 
  filename <- liftEff $ uriToFilename uri
  case state of
    ServerState { port: Just port, root: Just root } -> do
      -- TODO: Status Indication?
      { errors, success } <- rebuild port filename
      pure $ convertDiagnostics root settings errors
    _ -> pure emptyDiagnostics

censorWarnings :: Settings -> Array PscError -> Array PscError
censorWarnings settings = filter (flip notElem codes <<< getCode)
  where
    getCode (PscError { errorCode }) = errorCode
    codes = censorCodes settings
      
fullBuild :: forall eff. Notify (MainEff eff) -> DocumentStore -> Settings -> ServerState (MainEff eff) -> Array Foreign -> Aff (MainEff eff) DiagnosticResult
fullBuild logCb _ settings (ServerState { conn, root }) _ = do
  let command = buildCommand settings
  let buildCommand = either (const []) (\reg -> (split reg <<< trim) command) (regex "\\s+" noFlags)
  case conn, root, uncons buildCommand of
    Just conn', Just directory, Just { head: cmd, tail: args } -> do
      res <- build logCb { command: Command cmd args, directory, useNpmDir: addNpmPath settings }
      liftEff $ logCb Info "Build complete"
      pure $ convertDiagnostics directory settings res.errors
    _, _, _ -> do
      liftEff $ logCb Error "Error parsing build command"
      pure emptyDiagnostics
