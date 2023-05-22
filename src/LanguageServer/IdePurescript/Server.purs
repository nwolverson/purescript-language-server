module LanguageServer.IdePurescript.Server
  ( getEnvPursIdeSources
  , loadAll
  , retry
  , startServer'
  ) where

import Prelude

import Data.Array (filter, head)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String (null)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Utils (lines)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, attempt, delay, makeAff)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (Foreign)
import IdePurescript.Exec (findBins, getPathVar)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import IdePurescript.PscIdeServer as P
import LanguageServer.IdePurescript.Config (ConfigFn)
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.Protocol.Types (Settings)
import Node.Buffer (toString)
import Node.ChildProcess (defaultExecOptions, execFile)
import Node.Encoding (Encoding(..))
import Node.Process (lookupEnv)
import PscIde (load)
import PscIde.Server (Executable(..))

loadAll :: Int -> Aff (Either String Unit)
loadAll port = (either Left (const $ Right unit)) <$> load port [] []

retry :: Notify -> Int -> Aff Unit -> Aff Unit
retry logError n a
  | n > 0 = do
      res <- attempt a
      case res of
        Right r -> pure r
        Left err -> do
          liftEffect $ logError Info $ "Retrying starting server after 500ms: "
            <> show err
          delay (Milliseconds 500.0)
          retry logError (n - 1) a
retry _ _ a = a

getEnvPursIdeSources :: forall m. MonadEffect m => m (Maybe String)
getEnvPursIdeSources = liftEffect $ lookupEnv "PURS_IDE_SOURCES"

startServer' ::
  Settings ->
  String ->
  Notify ->
  Notify ->
  Aff { port :: Maybe Int, quit :: Aff Unit }
startServer' settings root cb logCb = do
  envIdeSources <- getEnvPursIdeSources
  packageGlobs <- case envIdeSources of
    Just sourcesString -> do
      liftEffect $ logCb Info "Using sources from PURS_IDE_SOURCES"
      pure (Regex.split (unsafeRegex """[\r\n\s]+""" noFlags) sourcesString)
    Nothing -> do
      liftEffect $ logCb Info
        "Using sources from psc-package/spago packages (PURS_IDE_SOURCES not set)"
      pscpGlob <- getPackagerPaths Config.addPscPackageSources "psc-package"
        settings
        root
      spagoGlob <- getPackagerPaths Config.addSpagoSources "spago" settings root
      pure (pscpGlob <> spagoGlob)
  P.startServer'
    { exe
    , combinedExe: true
    , glob: filter (not <<< null) $ globs <> packageGlobs
    , logLevel: Config.logLevel settings
    , outputDirectory: Just $ Config.effectiveOutputDirectory settings
    , port: Config.pscIdePort settings
    }
    root
    (Config.addNpmPath settings)
    cb
    logCb
  where
  globs = getGlob Config.srcPath <> getGlob Config.packagePath <>
    Config.sourceGlobs settings
  getGlob fn =
    fn settings
      # case _ of
          glob | not (null glob) -> [ glob <> "/**/*.purs" ]
          _ -> []
  exe = Config.pursExe settings

getPackagerPaths ::
  ConfigFn Boolean -> String -> Foreign -> String -> Aff (Array String)
getPackagerPaths enabled binName settings root =
  if not $ enabled settings then
    pure []
  else do
    pathVar <- liftEffect $ getPathVar (Config.addNpmPath settings) root
    serverBins <- findBins pathVar binName
    case head serverBins of
      Just (Executable bin _) ->
        makeAff \cb -> do
          void
            $ execFile bin [ "sources" ]
                (defaultExecOptions { cwd = Just root })
                ( \{ stdout } -> do
                    text <- toString UTF8 stdout
                    cb $ pure $ lines text
                )
          pure mempty
      _ -> pure []
