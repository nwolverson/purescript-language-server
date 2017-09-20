module LanguageServer.DocumentStore where

import Prelude
import Control.Monad.Eff (Eff)
import LanguageServer.TextDocument (TextDocument)
import LanguageServer.Types (CONN, DocumentStore, DocumentUri)

foreign import getDocuments :: forall eff. DocumentStore ->  Eff (conn :: CONN | eff) (Array TextDocument)

foreign import getDocument :: forall eff. DocumentStore -> DocumentUri -> Eff (conn :: CONN | eff) TextDocument

type TextDocumentChangeEvent = { document :: TextDocument }

foreign import onDidSaveDocument :: forall eff. DocumentStore -> (TextDocumentChangeEvent -> Eff (conn :: CONN | eff) Unit) -> Eff (conn :: CONN | eff) Unit
foreign import onDidOpenDocument :: forall eff. DocumentStore -> (TextDocumentChangeEvent -> Eff (conn :: CONN | eff) Unit) -> Eff (conn :: CONN | eff) Unit
foreign import onDidCloseDocument :: forall eff. DocumentStore -> (TextDocumentChangeEvent -> Eff (conn :: CONN | eff) Unit) -> Eff (conn :: CONN | eff) Unit
foreign import onDidChangeContent :: forall eff. DocumentStore -> (TextDocumentChangeEvent -> Eff (conn :: CONN | eff) Unit) -> Eff (conn :: CONN | eff) Unit
