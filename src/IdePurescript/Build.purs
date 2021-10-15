module IdePurescript.Build where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Array (intercalate, uncons, (:))
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (either, Either(..))
import Data.Maybe (maybe, Maybe(..))
import Data.String (Pattern(Pattern), indexOf, joinWith, split)
import Data.String as String
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, error, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (catchException)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object as Object
import IdePurescript.Exec (findBins, getPathVar)
import IdePurescript.PscErrors (PscResult(..), parsePscOutput)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import Node.ChildProcess (ChildProcess)
import Node.ChildProcess as CP
import Node.Encoding as Encoding
import Node.Process (getEnv)
import Node.Stream as S
import PscIde as P
import PscIde.Command (CodegenTarget, RebuildResult(..))
import PscIde.Server (Executable(Executable))

type BuildOptions
  = { command :: Command
    , directory :: String
    , useNpmDir :: Boolean
    }

data Command
  = Command String (Array String)

type BuildResult
  = { errors :: PscResult
    , success :: Boolean
    }

-- check if retrieved (copied) env object has "PATH" property, then use it,
-- otherwise use "Path" (for windows)
getPathProp :: Object.Object String -> String
getPathProp env =
  if Object.member "PATH" env then "PATH" else "Path"

-- Spawn with npm path, NO support for windows PATHEXT
spawn :: BuildOptions -> Effect ChildProcess
spawn { command: Command cmd args, directory, useNpmDir } = do
  env <-
    if useNpmDir then do
      pathVar <- getPathVar useNpmDir directory
      env <- getEnv
      pure $ Just $ Object.insert (getPathProp env) (either identity identity pathVar) env
    else
      pure Nothing
  CP.spawn cmd args (CP.defaultSpawnOptions { cwd = Just directory, env = env })

-- Spawn with npm path, "which" call (windows support) and version info gathering
spawnWithVersion :: BuildOptions -> Aff { cmdBins :: Array Executable, cp :: Maybe ChildProcess }
spawnWithVersion { command: Command cmd args, directory, useNpmDir } = do
  pathVar <- liftEffect $ getPathVar useNpmDir directory
  cmdBins <- findBins pathVar cmd
  cp <-
    liftEffect
      $ case uncons cmdBins of
          Just { head: Executable cmdBin _ } -> do
            env <- liftEffect getEnv
            let childEnv = Object.insert (getPathProp env) (either identity identity pathVar) env
            Just <$> CP.spawn cmdBin args (CP.defaultSpawnOptions { cwd = Just directory, env = Just childEnv })
          _ -> pure Nothing
  pure { cmdBins, cp }

build :: Notify -> BuildOptions -> Aff (Either String BuildResult)
build logCb buildOptions@{ command: Command cmd args } = do
  { cmdBins, cp: cp' } <- spawnWithVersion buildOptions
  makeAff
    $ \cb -> do
        let
          succ = cb <<< Right
          err = cb <<< Left
        logCb Info $ "Resolved build command (1st is used): "
        traverse_
          ( \(Executable x vv) -> do
              logCb Info $ x <> maybe "" (": " <> _) vv
          )
          cmdBins
        case cp' of
          Nothing -> succ $ Left $ "Didn't find command in PATH: " <> cmd
          Just cp -> do
            logCb Info $ "Running build command: " <> intercalate " " (cmd : args)
            CP.onError cp (cb <<< Left <<< CP.toStandardError)
            errOutput <- Ref.new ""
            outOutput <- Ref.new ""
            let
              res :: Ref String -> String -> Effect Unit
              res r s = Ref.modify_ (_ <> s) r
            catchException err $ S.onDataString (CP.stderr cp) Encoding.UTF8 (res errOutput)
            catchException err $ S.onDataString (CP.stdout cp) Encoding.UTF8 (res outOutput)
            CP.onClose cp
              ( \exit -> case exit of
                  CP.Normally n
                    | n == 0 || n == 1 -> do
                      pursError <- Ref.read (errOutput)
                      pursOutput <- Ref.read (outOutput)
                      let
                        lines = split (Pattern "\n") $ pursError <> pursOutput
                        { yes: json, no: toLog } = Array.partition (\s -> indexOf (Pattern "{\"") s == Just 0) lines
                      logCb Info $ joinWith "\n" toLog
                      case parsePscOutput <$> json of
                        [ Left e ] -> succ $ Left $ "Couldn't parse build output: " <> e
                        [ Right r ] -> succ $ Right { errors: r, success: n == 0 }
                        [] ->
                          succ
                            $ Left
                            $ "Problem running build: "
                            <> if String.length pursError > 0 then
                                String.take 500 pursError
                              else
                                "didn't find JSON output"
                        _ -> succ $ Left "Found multiple lines of JSON output, don't know what to do"
                  _ -> succ $ Left "Build process exited abnormally"
              )
        pure mempty

rebuild :: Int -> String -> Maybe (Array CodegenTarget) -> Aff BuildResult
rebuild port file targets = do
  res <- P.rebuild port file (Just file) targets
  either
    (throwError <<< error)
    (pure <<< onResult)
    res
  where

  onResult :: Either RebuildResult RebuildResult -> BuildResult
  onResult =
    either (\errors -> { errors: PscResult { errors, warnings: [] }, success: true })
      (\warnings -> { errors: PscResult { errors: [], warnings }, success: true })
      <<< bimap unwrap unwrap
    where
    unwrap (RebuildResult r) = r
