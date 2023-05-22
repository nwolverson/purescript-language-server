module LanguageServer.IdePurescript.Formatting
  ( getFormattedDocument
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
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
import LanguageServer.Protocol.Uri (uriToFilename)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.Encoding as Encoding
import Node.Stream as S

getFormattedDocument ::
  Notify ->
  DocumentStore ->
  Settings ->
  ServerState ->
  DocumentFormattingParams ->
  Aff (Array TextEdit)
getFormattedDocument
  logCb
  docs
  settings
  serverState
  { textDocument: TextDocumentIdentifier textDocId } = do
  case Config.formatter settings of
    NoFormatter -> do
      liftEffect
        $ logCb
            Warning
            "Trying to format document: no `formatter` value set in settings!"
      pure []
    formatter -> do
      maybeDoc <- liftEffect $ Nullable.toMaybe <$> getDocument docs
        textDocId.uri
      case maybeDoc of
        Nothing -> pure []
        Just doc -> do
          file <- liftEffect $ uriToFilename textDocId.uri
          liftEffect
            $ logCb Info
            $ "Formatting document " <> file <> " with "
                <> (formatCmd formatter # \(Command cmd _) -> cmd)
          text <- liftEffect $ getText doc
          newTextEither <- attempt $ format logCb settings serverState formatter
            text
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

formatCmd :: Formatter -> Command
formatCmd = case _ of
  Purty -> purtyCommand
  PursTidy -> pursTidyCommand
  Pose -> poseCommand
  NoFormatter -> Command "echo" [] -- Not possible

-- TODO for NoFormatter don't provide formatting provider
format :: Notify -> Settings -> ServerState -> Formatter -> String -> Aff String
format _logCb settings state formatter text = do
  let
    command@(Command _cmd _) = formatCmd formatter
  case state of
    ServerState { root: Just directory } -> do
      makeAff
        $ \cb -> do
            let
              succ = cb <<< Right
              err = cb <<< Left
            cp <- spawn
              { command
              , directory
              , useNpmDir: Config.addNpmPath settings
              }
            CP.onError cp (err <<< CP.toStandardError)
            result <- Ref.new []
            let
              res :: Buffer -> Effect Unit
              res s = Ref.modify_ (_ `Array.snoc` s) result
            catchException err $ S.onDataString (CP.stderr cp) Encoding.UTF8 $
              err <<< error
            catchException err $ S.onData (CP.stdout cp) res
            CP.onClose cp \exit -> case exit of
              CP.Normally n
                | n == 0 || n == 1 ->
                    Ref.read result >>= Buffer.concat
                      >>= Buffer.toString Encoding.UTF8
                      >>= succ
              _ -> do
                let Command cmd _ = command
                err $ error $ cmd <> " process exited abnormally"
            when (not $ Foreign.isUndefined $ unsafeToForeign $ CP.pid cp) do
              catchException err $ void $ S.writeString (CP.stdin cp) UTF8 text
                ( const
                    $ pure unit
                )
              catchException err $ S.end (CP.stdin cp) (const $ pure unit)
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
