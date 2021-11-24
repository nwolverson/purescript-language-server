module LanguageServer.IdePurescript.Formatting
  ( getFormattedDocument
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Aff (Aff, attempt, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (catchException, error)
import Effect.Ref as Ref
import Foreign (unsafeToForeign)
import Foreign as Foreign
import IdePurescript.Build (Command(..), spawn)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import LanguageServer.IdePurescript.Config (Formatter(..))
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (DocumentFormattingParams)
import LanguageServer.Protocol.TextDocument (getText)
import LanguageServer.Protocol.Types (DocumentStore, Position(..), Range(..), Settings, TextDocumentIdentifier(..), TextEdit(..))
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.Encoding as Encoding
import Node.Stream as S

getFormattedDocument :: Notify -> DocumentStore -> Settings -> ServerState -> DocumentFormattingParams -> Aff (Array TextEdit)
getFormattedDocument logCb docs settings serverState { textDocument: TextDocumentIdentifier textDocId } = do
  case Config.formatter settings of
    NoFormatter -> pure []
    formatter -> do
      text <- liftEffect $ getText =<< getDocument docs textDocId.uri
      newTextEither <- attempt $ format logCb settings serverState formatter text
      case newTextEither of
        Left err -> liftEffect (logCb Error $ show err) $> []
        Right "" -> pure []
        Right newText -> pure [ mkTextEdit text newText ]

purtyCommand :: Command
purtyCommand = Command "purty" [ "format", "-" ]
pursTidyCommand :: Command
pursTidyCommand = Command "purs-tidy" [ "format" ]
poseCommand :: Command
poseCommand = Command "prettier" [ "--parser", "purescript" ]

-- TODO for NoFormatter don't provide formatting provider 
format :: Notify -> Settings -> ServerState -> Formatter -> String -> Aff String
format logCb settings state formatter text = do
  let
    command = case formatter of
      Purty -> purtyCommand
      PursTidy -> pursTidyCommand
      Pose -> poseCommand
      NoFormatter -> Command "echo" [] -- Not possible
    Command cmd _ = command
  case state of
    ServerState { root: Just directory } -> do
      makeAff
        $ \cb -> do
            let
              succ = cb <<< Right
              err = cb <<< Left
            cp <- spawn { command, directory, useNpmDir: Config.addNpmPath settings }
            CP.onError cp (err <<< CP.toStandardError)
            result <- Ref.new ""
            let
              res :: String -> Effect Unit
              res s = Ref.modify_ (_ <> s) result
            catchException err $ S.onDataString (CP.stderr cp) Encoding.UTF8 $ err <<< error
            catchException err $ S.onDataString (CP.stdout cp) Encoding.UTF8 res
            CP.onClose cp \exit -> case exit of
              CP.Normally n
                | n == 0 || n == 1 ->
                  Ref.read result >>= succ
              _ -> do
                let Command cmd _ = command
                err $ error $ cmd <> " process exited abnormally"
            when (not $ Foreign.isUndefined $ unsafeToForeign $ CP.pid cp) do
              catchException err $ void $ S.writeString (CP.stdin cp) UTF8 text (pure unit)
              catchException err $ S.end (CP.stdin cp) (pure unit)
            pure mempty
    _ -> pure ""

mkTextEdit :: String -> String -> TextEdit
mkTextEdit oldText text = TextEdit { range, newText: text }
  where
  range =
    Range
      { start: Position { line: 0, character: 0 }
      , end: Position { line: (length $ lines oldText) + 1, character: 0 }
      }
