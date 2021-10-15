module LanguageServer.DocumentStore where

import Prelude
import Effect (Effect)
import LanguageServer.TextDocument (TextDocument)
import LanguageServer.Types (DocumentStore, DocumentUri)

foreign import getDocuments :: DocumentStore -> Effect (Array TextDocument)

foreign import getDocument :: DocumentStore -> DocumentUri -> Effect TextDocument

type TextDocumentChangeEvent
  = { document :: TextDocument }

foreign import onDidSaveDocument :: DocumentStore -> (TextDocumentChangeEvent -> Effect Unit) -> Effect Unit
foreign import onDidOpenDocument :: DocumentStore -> (TextDocumentChangeEvent -> Effect Unit) -> Effect Unit
foreign import onDidCloseDocument :: DocumentStore -> (TextDocumentChangeEvent -> Effect Unit) -> Effect Unit
foreign import onDidChangeContent :: DocumentStore -> (TextDocumentChangeEvent -> Effect Unit) -> Effect Unit
