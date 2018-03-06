module IdePurescript.PscIdeServer
  ( startServer
  , startServer'
  , stopServer
  , ServerStartResult(..)
  , ServerEff
  , Port
  , QuitCallback
  , ErrorLevel(..)
  , Notify(..)
  ) where

import Prelude

import Control.Monad.Aff (Aff, attempt, liftEff')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (length, head)
import Data.Either (either)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.String (Pattern(Pattern), split, toLower)
import Data.Traversable (traverse, traverse_)
import Global (readInt)
import IdePurescript.Exec (getPathVar, findBins)
import IdePurescript.PscIde (cwd) as PscIde
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS, ChildProcess, stderr, stdout)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.Path (normalize)
import Node.Platform (Platform(..))
import Node.Process (PROCESS, platform)
import Node.Stream (onDataString)
import PscIde (NET)
import PscIde.Server (Executable(Executable), LogLevel, defaultServerArgs, getSavedPort, pickFreshPort, savePort)
import PscIde.Server as S

type Port = Int

data ServerStartResult =
    CorrectPath Port
  | WrongPath Port String
  | Started Port ChildProcess
  | Closed
  | StartError String

type ServerEff eff = (cp :: CHILD_PROCESS, process :: PROCESS, net :: NET, avar :: AVAR, fs :: FS, exception :: EXCEPTION, random :: RANDOM, buffer :: BUFFER | eff)

type QuitCallback eff = (Aff (net :: NET, cp :: CHILD_PROCESS, fs :: FS | eff) Unit)

data ErrorLevel = Success | Info | Warning | Error
type Notify eff = ErrorLevel -> String -> Eff eff Unit

data Version = Version Int Int Int

parseVersion :: String -> Maybe Version
parseVersion s =
  case traverse fromNumber $ readInt 10 <$> split (Pattern ".") s of
    Just [a, b, c] -> Just $ Version a b c
    _ -> Nothing

instance eqVersion :: Eq Version where
  eq (Version a b c) (Version a' b' c') = a == a' && b == b' && c == c'

instance ordVersion :: Ord Version where
  compare (Version a b c) (Version a' b' c') = compare [a,b,c] [a',b',c']

instance showVersion :: Show Version where
  show (Version a b c) = show a <> "." <> show b <> "." <> show c

type ServerSettings =
  { exe :: String
  , combinedExe :: Boolean
  , glob :: Array String
  , logLevel :: Maybe LogLevel
  , editorMode :: Boolean
  , polling :: Boolean
  , outputDirectory :: Maybe String
  }

-- | Start a psc-ide server instance, or find one already running on the expected port, checking if it has the right path.
-- | Will notify as to what is happening, choose to supply globs appropriately
startServer' :: forall eff eff'.
  ServerSettings
  -> String
  -> Boolean
  -> Notify (ServerEff eff)
  -> Notify (ServerEff eff)
  -> Aff (ServerEff eff) { quit :: QuitCallback eff', port :: Maybe Int }
startServer' settings@({ exe: server, glob }) path addNpmBin cb logCb = do
  pathVar <- liftEff $ getPathVar addNpmBin path
  serverBins <- findBins pathVar server
  case head serverBins of
    Nothing -> do
      liftEff $ cb Info $ "Couldn't find IDE server, check PATH. Looked for: "
        <> server <> " in PATH: " <> either id id pathVar
      pure { quit: pure unit, port: Nothing }
    Just (Executable bin version) -> do
      liftEff $ logCb Info $ "Resolved IDE server paths (npm-bin: " <> show addNpmBin <> ") from PATH of " <> either id id pathVar <> " (1st is used):"
      traverse_ (\(Executable x vv) ->
        liftEff $ logCb Info $ x <> ": " <> fromMaybe "ERROR" vv) serverBins
      liftEff $ when (length serverBins > 1) $ cb Warning $ "Found multiple IDE server executables; using " <> bin
      res <- startServer logCb (settings { exe = bin }) path
      let noRes = { quit: pure unit, port: Nothing }
      liftEff $ case res of
        CorrectPath usedPort -> { quit: pure unit, port: Just usedPort } <$ cb Info ("Found existing IDE server with correct path on port " <> show usedPort)
        WrongPath usedPort wrongPath -> do
          cb Error $ "Found existing IDE server on port '" <> show usedPort <> "' with wrong path: '" <> wrongPath
            <> "'. Correct, kill or configure a different port, and restart."
          pure noRes
        Started usedPort cp -> do
          cb Success $ "Started IDE server (port " <> show usedPort <> ")"
          wireOutput cp logCb
          pure
            { quit: stopServer usedPort path cp
            , port: Just usedPort
            }
        Closed -> noRes <$ cb Info "IDE server exited with success code"
        StartError err -> noRes <$ (cb Error $ "Could not start IDE server process. Check the configured port number is valid.\n" <> err)
  where
    wireOutput :: ChildProcess -> Notify (ServerEff eff) -> Eff (ServerEff eff) Unit
    wireOutput cp log = do
      onDataString (stderr cp) UTF8 (log Warning)
      onDataString (stdout cp) UTF8 (log Info)

-- | Start a psc-ide server instance, or find one already running on the expected port, checking if it has the right path.
startServer :: forall eff. Notify (ServerEff eff) -> ServerSettings -> String -> Aff (ServerEff eff) ServerStartResult
startServer logCb { exe, combinedExe, glob, logLevel, editorMode, polling, outputDirectory } rootPath = do
  port <- liftEff $ getSavedPort rootPath
  case port of
    Just p -> do
      workingDir <- attempt $ PscIde.cwd p
      liftEff $ logCb Info $ "Found existing port from file: " <> show p <> (either (const "") (", cwd: " <> _) workingDir)
      either (const launchServer) (gotPath p) workingDir
    Nothing -> launchServer

  where
  launchServer = do
    newPort <- liftEff pickFreshPort
    liftEff $ do
      logCb Info $ "Starting IDE server on port " <> show newPort <> " with cwd " <> rootPath
      savePort newPort rootPath
    r newPort <$> S.startServer (defaultServerArgs
      { exe = exe
      , combinedExe = combinedExe
      , cwd = Just rootPath
      , port = Just newPort
      , source = glob
      , logLevel = logLevel
      , editorMode = editorMode
      , polling = polling
      , outputDirectory = outputDirectory
      })
    where
      r newPort (S.Started cp) = Started newPort cp
      r _ (S.Closed) = Closed
      r _ (S.StartError s) = StartError s

  gotPath port workingDir =
    liftEff $ if normalizePath workingDir == normalizePath rootPath then
        do
          logCb Info $ "Found IDE server on port " <> show port <> " with correct path: " <> workingDir
          pure $ CorrectPath port
      else
        do
          logCb Info $ "Found IDE server on port " <> show port <> " with wrong path: " <> normalizePath workingDir <> " instead of " <> normalizePath rootPath
          pure $ WrongPath port workingDir

  normalizePath = (if platform == Just Win32 then toLower else id) <<< normalize

-- | Stop a psc-ide server. Currently implemented by asking it nicely, but potentially by killing it if that doesn't work...
stopServer :: forall eff. Int -> String -> ChildProcess -> Aff (cp :: CHILD_PROCESS, net :: NET, fs :: FS | eff) Unit
stopServer port rootPath cp = do
  oldPort <- liftEff $ S.getSavedPort rootPath
  _ <- liftEff' $ when (oldPort == Just port) $ S.deleteSavedPort rootPath
  S.stopServer port
