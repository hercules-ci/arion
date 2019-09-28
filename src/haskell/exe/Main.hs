{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

import Protolude hiding (Down, option)

import           Arion.Nix
import           Arion.Aeson
import           Arion.Images (loadImages)
import qualified Arion.DockerCompose as DockerCompose
import           Arion.Services (getDefaultExec)

import Options.Applicative
import Control.Applicative
import Control.Monad.Fail

import qualified Data.Aeson.Encode.Pretty
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

import Control.Arrow ((>>>))

import System.Posix.User (getRealUserID)

data CommonOptions =
  CommonOptions
    { files :: NonEmpty FilePath
    , pkgs :: Text
    , nixArgs :: [Text]
    }
  deriving (Show)

newtype DockerComposeArgs =
  DockerComposeArgs { unDockerComposeArgs :: [Text] }

ensureConfigFile :: [FilePath] -> NonEmpty FilePath
ensureConfigFile []     = "./arion-compose.nix" :| []
ensureConfigFile (x:xs) = x :| xs

parseOptions :: Parser CommonOptions
parseOptions = do
    files <-
      ensureConfigFile <$>
        many (strOption
               (  short 'f'
               <> long "file"
               <> metavar "FILE"
               <> help "Use FILE instead of the default ./arion-compose.nix. \
                        \Can be specified multiple times for a merged configuration" ))
    pkgs <- T.pack <$> strOption
          (  short 'p'
          <> long "pkgs"
          <> metavar "EXPR"
          <> showDefault
          <> value "./arion-pkgs.nix"
          <> help "Use Nix expression EXPR to get the Nixpkgs attrset used for bootstrapping \
                   \and evaluating the configuration." )
    showTrace <- flag False True (long "show-trace"
                    <> help "Causes Nix to print out a stack trace in case of Nix expression evaluation errors.")
    -- TODO --option support (https://github.com/pcapriotti/optparse-applicative/issues/284)
    userNixArgs <- many (T.pack <$> strOption (long "nix-arg" <> metavar "ARG" <> help "Pass an extra argument to nix. Example: --nix-arg --option --nix-arg substitute --nix-arg false"))
    pure $
      let nixArgs = userNixArgs <|> "--show-trace" <$ guard showTrace
      in CommonOptions{..}

textArgument = fmap T.pack . strArgument

parseCommand :: Parser (CommonOptions -> IO ())
parseCommand =
  hsubparser
    (    command "cat" (info (pure runCat) (progDesc "Spit out the docker compose file as JSON" <> fullDesc))
      <> command "repl" (info (pure runRepl) (progDesc "Start a nix repl for the whole composition" <> fullDesc))
      <> command "exec" (info (parseExecCommand) (progDesc "Execute a command in a running container" <> fullDesc))
    )
  <|>
  hsubparser
    (    commandDC runBuildAndDC "build" "Build or rebuild services"
      <> commandDC runBuildAndDC "bundle" "Generate a Docker bundle from the Compose file"
      <> commandDC runEvalAndDC "config" "Validate and view the Compose file"
      <> commandDC runBuildAndDC "create" "Create services"
      <> commandDC runEvalAndDC "down" "Stop and remove containers, networks, images, and volumes"
      <> commandDC runEvalAndDC "events" "Receive real time events from containers"
      <> commandDC runDC "help" "Get help on a command"
      <> commandDC runEvalAndDC "images" "List images"
      <> commandDC runEvalAndDC "kill" "Kill containers"
      <> commandDC runEvalAndDC "logs" "View output from containers"
      <> commandDC runEvalAndDC "pause" "Pause services"
      <> commandDC runEvalAndDC "port" "Print the public port for a port binding"
      <> commandDC runEvalAndDC "ps" "List containers"
      <> commandDC runBuildAndDC "pull" "Pull service images"
      <> commandDC runBuildAndDC "push" "Push service images"
      <> commandDC runBuildAndDC "restart" "Restart services"
      <> commandDC runEvalAndDC "rm" "Remove stopped containers"
      <> commandDC runBuildAndDC "run" "Run a one-off command"
      <> commandDC runBuildAndDC "scale" "Set number of containers for a service"
      <> commandDC runBuildAndDC "start" "Start services"
      <> commandDC runEvalAndDC "stop" "Stop services"
      <> commandDC runEvalAndDC "top" "Display the running processes"
      <> commandDC runEvalAndDC "unpause" "Unpause services"
      <> commandDC runBuildAndDC "up" "Create and start containers"
      <> commandDC runDC "version" "Show the Docker-Compose version information"

      <> metavar "DOCKER-COMPOSE-COMMAND"
      <> commandGroup "Docker Compose Commands:"
    )

parseAll :: Parser (IO ())
parseAll =
  flip ($) <$> parseOptions <*> parseCommand

parseDockerComposeArgs :: Parser DockerComposeArgs
parseDockerComposeArgs =
  DockerComposeArgs <$>
    many (argument (T.pack <$> str) (metavar "DOCKER-COMPOSE ARGS..."))

commandDC
  :: (Text -> DockerComposeArgs -> CommonOptions -> IO ())
  -> Text
  -> Text
  -> Mod CommandFields (CommonOptions -> IO ())
commandDC run cmdStr help =
  command
    (T.unpack cmdStr)
    (info
      (run cmdStr <$> parseDockerComposeArgs)
      (progDesc (T.unpack help) <> fullDesc <> forwardOptions))


--------------------------------------------------------------------------------

runDC :: Text -> DockerComposeArgs -> CommonOptions -> IO ()
runDC cmd (DockerComposeArgs args) opts = do
  DockerCompose.run DockerCompose.Args
    { files = []
    , otherArgs = [cmd] ++ args
    }

runBuildAndDC :: Text -> DockerComposeArgs -> CommonOptions -> IO ()
runBuildAndDC cmd dopts opts = do
  ea <- defaultEvaluationArgs opts
  Arion.Nix.withBuiltComposition ea $ \path -> do
    loadImages path
    DockerCompose.run DockerCompose.Args
      { files = [path]
      , otherArgs = [cmd] ++ unDockerComposeArgs dopts
      }

runEvalAndDC :: Text -> DockerComposeArgs -> CommonOptions -> IO ()
runEvalAndDC cmd dopts opts = do
  ea <- defaultEvaluationArgs opts
  Arion.Nix.withEvaluatedComposition ea $ \path ->
    DockerCompose.run DockerCompose.Args
      { files = [path]
      , otherArgs = [cmd] ++ unDockerComposeArgs dopts
      }

defaultEvaluationArgs :: CommonOptions -> IO EvaluationArgs
defaultEvaluationArgs co = do
  uid <- getRealUserID
  pure EvaluationArgs
    { evalUid = fromIntegral uid
    , evalModules = files co
    , evalPkgs = pkgs co
    , evalWorkDir = Nothing
    , evalMode = ReadWrite
    , evalUserArgs = nixArgs co
    }

runCat :: CommonOptions -> IO ()
runCat co = do
  v <- Arion.Nix.evaluateComposition =<< defaultEvaluationArgs co
  T.hPutStrLn stdout (pretty v)

runRepl :: CommonOptions -> IO ()
runRepl co = do
  putErrText
    "Launching a repl for you. To get started:\n\
    \\n\
    \To see deployment-wide configuration\n\
    \  type config. and hit TAB\n\
    \To see the services\n\
    \  type config.docker-compose.evaluatedServices TAB or ENTER\n\
    \To bring the top-level Nixpkgs attributes into scope\n\
    \  type :a (config._module.args.pkgs) // { inherit config; }\n\
    \"
  Arion.Nix.replForComposition =<< defaultEvaluationArgs co

detachFlag :: Parser Bool
detachFlag = flag False True (long "detach" <> short 'd' <> help "Detached mode: Run command in the background.")

privilegedFlag :: Parser Bool
privilegedFlag = flag False True (long "privileged" <> help "Give extended privileges to the process.")

userOption :: Parser Text
userOption = strOption (long "user" <> short 'u' <> help "Run the command as this user.")

noTTYFlag :: Parser Bool
noTTYFlag = flag False True (short 'T' <> help "Disable pseudo-tty allocation. By default `exec` allocates a TTY.")

indexOption :: Parser Int
indexOption = option
  (auto >>= \i -> i <$ unless (i >= 1) (fail "container index must be >= 1"))
  (long "index" <> value 1 <> help "Index of the container if there are multiple instances of a service.")

envOption :: Parser (Text, Text)
envOption = option (auto >>= spl) (long "env" <> short 'e' <> help "Set environment variables (can be used multiple times, not supported in Docker API < 1.25)")
  where spl s = case T.break (== '=') s of
          (_, "") -> fail "--env parameter needs to combine key and value with = sign"
          (k, ev) -> pure (k, T.drop 1 ev)

workdirOption :: Parser Text
workdirOption = strOption (long "workdir" <> short 'w' <> metavar "DIR" <> help "Working directory in which to start the command in the container.")

parseExecCommand :: Parser (CommonOptions -> IO ())
parseExecCommand = runExec
  <$> detachFlag
  <*> privilegedFlag
  <*> optional userOption
  <*> noTTYFlag
  <*> indexOption
  <*> many envOption
  <*> optional workdirOption
  <*> textArgument (metavar "SERVICE")
  <*> orEmpty' (
      (:) <$> argument (T.pack <$> str) (metavar "COMMAND")
          <*> many (argument (T.pack <$> str) (metavar "ARG"))
  )

orEmpty' :: (Alternative f, Monoid a) => f a -> f a
orEmpty' m = fromMaybe mempty <$> optional m

runExec :: Bool -> Bool -> Maybe Text -> Bool -> Int -> [(Text, Text)] -> Maybe Text -> Text -> [Text] -> CommonOptions -> IO ()
runExec detach privileged user noTTY index envs workDir service commandAndArgs opts = do
  putErrText $ "Service: " <> service

  ea <- defaultEvaluationArgs opts
  Arion.Nix.withEvaluatedComposition ea $ \path -> do
    commandAndArgs'' <- case commandAndArgs of
      [] -> getDefaultExec path service
      x -> pure x
    let commandAndArgs' = case commandAndArgs'' of
          [] -> ["/bin/sh"]
          x -> x
  
    let args = concat
          [ ["exec"]
          , ("--detach" <$ guard detach :: [Text])
          , "--privileged" <$ guard privileged
          , "-T" <$ guard noTTY
          , (\(k, v) -> ["--env", k <> "=" <> v]) =<< envs
          , join $ toList (user <&> \u -> ["--user", u])
          , ["--index", show index]
          , join $ toList (workDir <&> \w -> ["--workdir", w])
          , [service]
          , commandAndArgs'
          ]
    DockerCompose.run DockerCompose.Args
      { files = [path]
      , otherArgs = args
      }

main :: IO ()
main = 
  (join . execParser) (info (parseAll <**> helper) fullDesc)
  where
    execParser = customExecParser (prefs showHelpOnEmpty)

