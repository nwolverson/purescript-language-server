module IdePurescript.PscIdeServer
  ( startServer
  , startServer'
  , stopServer
  , ServerStartResult(..)
  , Port
  , ErrorLevel(..)
  , Notify(..)
  ) where

import Prelude
import Data.Array (length, head)
import Data.Either (either)
import Data.Int as Int
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isNothing, maybe)
import Data.String (Pattern(Pattern), split, toLower)
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, attempt, try)
import Effect.Class (liftEffect)
import IdePurescript.Exec (getPathVar, findBins)
import IdePurescript.PscIde (cwd) as PscIde
import Node.ChildProcess (ChildProcess, stderr, stdout)
import Node.Encoding (Encoding(..))
import Node.Path (normalize)
import Node.Platform (Platform(..))
import Node.Process (platform)
import Node.Stream (onDataString)
import PscIde.Server (Executable(Executable), LogLevel, defaultServerArgs, getSavedPort, pickFreshPort, savePort)
import PscIde.Server as S

type Port
  = Int

data ServerStartResult
  = CorrectPath Port
  | WrongPath Port String String
  | Started Port ChildProcess
  | Closed
  | StartError String

data ErrorLevel
  = Success
  | Info
  | Warning
  | Error
instance showErrorLevel :: Show ErrorLevel where
  show Success = "Success"
  show Info = "Info"
  show Warning = "Warning"
  show Error = "Error"

type Notify
  = ErrorLevel -> String -> Effect Unit

data Version
  = Version Int Int Int

parseVersion :: String -> Maybe Version
parseVersion s =
  case traverse Int.fromString $ split (Pattern ".") s of
    Just [ a, b, c ] -> Just $ Version a b c
    _ -> Nothing

instance eqVersion :: Eq Version where
  eq (Version a b c) (Version a' b' c') = a == a' && b == b' && c == c'

instance ordVersion :: Ord Version where
  compare (Version a b c) (Version a' b' c') = compare [ a, b, c ] [ a', b', c' ]

instance showVersion :: Show Version where
  show (Version a b c) = show a <> "." <> show b <> "." <> show c

type ServerSettings
  = { exe :: String
    , combinedExe :: Boolean
    , glob :: Array String
    , logLevel :: Maybe LogLevel
    , outputDirectory :: Maybe String
    , port :: Maybe Int
    }

-- | Start a psc-ide server instance, or find one already running on the expected port, checking if it has the right path.
-- | Will notify as to what is happening, choose to supply globs appropriately
startServer' ::
  ServerSettings ->
  String ->
  Boolean ->
  Notify ->
  Notify ->
  Aff { quit :: Aff Unit, port :: Maybe Int }
startServer' settings@({ exe: server }) path addNpmBin cb logCb = do
  pathVar <- liftEffect $ getPathVar addNpmBin path
  serverBins <- findBins pathVar server
  case head serverBins of
    Nothing -> do
      liftEffect
        $ cb Info
        $ "Couldn't find IDE server, check PATH. Looked for: "
        <> server
        <> " in PATH: "
        <> either identity identity pathVar
      pure { quit: pure unit, port: Nothing }
    Just (Executable bin _version) -> do
      liftEffect $ logCb Info $ "Resolved IDE server paths (npm-bin: " <> show addNpmBin <> ") from PATH of " <> either identity identity pathVar <> " (1st is used):"
      traverse_
        ( \(Executable x vv) ->
            liftEffect $ logCb Info $ x <> ": " <> fromMaybe "ERROR" vv
        )
        serverBins
      liftEffect $ when (length serverBins > 1) $ cb Warning $ "Found multiple IDE server executables; using " <> bin
      res <- startServer logCb (settings { exe = bin }) path
      let noRes = { quit: pure unit, port: Nothing }
      liftEffect
        $ case res of
            CorrectPath usedPort -> { quit: pure unit, port: Just usedPort } <$ cb Info ("Found existing IDE server with correct path on port " <> show usedPort)
            WrongPath usedPort wrongPath expectedPath -> do
              cb Error
                $ "Found existing IDE server on port '"
                <> show usedPort
                <> "' with wrong path: '"
                <> wrongPath
                <> "' instead of '"
                <> expectedPath
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
  wireOutput :: ChildProcess -> Notify -> Effect Unit
  wireOutput cp log = do
    onDataString (stderr cp) UTF8 (log Warning)
    onDataString (stdout cp) UTF8 (log Info)

-- | Start a psc-ide server instance, or find one already running on the expected port, checking if it has the right path.
startServer :: Notify -> ServerSettings -> String -> Aff ServerStartResult
startServer logCb { exe, combinedExe, glob, logLevel, outputDirectory, port: configuredPort } rootPath = do
  case configuredPort of
    -- Connect to existing server or launch one on this port
    Just port -> joinServer port configuredPort "Using configured port"
    Nothing ->
      (liftEffect $ getSavedPort rootPath)
        >>= case _ of
            -- Connect to existing server on this port or launch one on new port
            Just port -> joinServer port Nothing "Found existing port from file"
            -- Launch on new port
            Nothing -> launchServer Nothing

  where

  joinServer :: Int -> Maybe Int -> String -> Aff ServerStartResult
  joinServer port launchPort message = do
    workingDir <- attempt $ PscIde.cwd port
    liftEffect $ logCb Info $ message <> ": " <> show port <> (either (const " (couldn't connect to existing server)") (", cwd: " <> _) workingDir)
    either (const $ launchServer launchPort) (gotPath port) workingDir

  launchServer connectPort = do
    port <- maybe (liftEffect pickFreshPort) pure connectPort
    liftEffect
      $ do
          logCb Info $ "Starting IDE server on port " <> show port <> " with cwd " <> rootPath
          -- Save it just if we picked it
          when (isNothing connectPort) $ savePort port rootPath
    r port
      <$> S.startServer
          ( defaultServerArgs
              { exe = exe
              , combinedExe = combinedExe
              , cwd = Just rootPath
              , port = Just port
              , source = glob
              , logLevel = logLevel
              , outputDirectory = outputDirectory
              }
          )
    where
    r port (S.Started cp) = Started port cp
    r _ (S.Closed) = Closed
    r _ (S.StartError s) = StartError s

  gotPath port workingDir =
    liftEffect
      $ if normalizePath workingDir == normalizePath rootPath then do
          logCb Info $ "Found IDE server on port " <> show port <> " with correct path: " <> workingDir
          pure $ CorrectPath port
        else do
          logCb Info $ "Found IDE server on port " <> show port <> " with wrong path: " <> normalizePath workingDir <> " instead of " <> normalizePath rootPath
          pure $ WrongPath port workingDir rootPath

  normalizePath = (if platform == Just Win32 then toLower else identity) <<< normalize

-- | Stop a psc-ide server. Currently implemented by asking it nicely, but potentially by killing it if that doesn't work...
stopServer :: Int -> String -> ChildProcess -> Aff Unit
stopServer port rootPath _cp = do
  oldPort <- liftEffect $ S.getSavedPort rootPath
  _ <- try $ liftEffect $ when (oldPort == Just port) $ S.deleteSavedPort rootPath
  S.stopServer port
