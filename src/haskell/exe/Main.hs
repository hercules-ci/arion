{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

import Protolude hiding (Down)

import Options.Applicative
import Control.Applicative

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

import Control.Arrow ((>>>))


data CommonOptions =
  CommonOptions
    { files :: NonEmpty FilePath
    , pkgs :: Text
    }
  deriving (Show)


newtype DockerComposeArgs =
  DockerComposeArgs { unDockerComposeArgs :: [Text] }


data DockerComposeCmd =
    Build
  | Bundle
  | Config
  | Create
  | Down
  | Events
  | Exec
  | Help
  | Images
  | Kill
  | Logs
  | Pause
  | Port
  | Ps
  | Pull
  | Push
  | Restart
  | Rm
  | Run
  | Scale
  | Start
  | Stop
  | Top
  | Unpause
  | Up
  | Version
  deriving (Show)



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
          <> help "Use EXPR to get the Nixpkgs attrset used for bootstrapping \
                   \and evaluating the configuration." )
    pure CommonOptions{..}


parseCommand :: Parser (CommonOptions -> IO ())
parseCommand =
  hsubparser
    (    command "cat" (info (pure runCat) fullDesc)
      <> command "repl" (info (pure runRepl) fullDesc)
      <> command "exec" (info (pure runExec) fullDesc)
    )
  <|>
  hsubparser
    (    dcParser "build" Build "Build or rebuild services"
      <> dcParser "bundle" Bundle "Generate a Docker bundle from the Compose file"
      <> dcParser "config" Config "Validate and view the Compose file"
      <> dcParser "create" Create "Create services"
      <> dcParser "down" Down "Stop and remove containers, networks, images, and volumes"
      <> dcParser "events" Events "Receive real time events from containers"
      <> dcParser "exec" Exec "Execute a command in a running container"
      <> dcParser "help" Help "Get help on a command"
      <> dcParser "images" Images "List images"
      <> dcParser "kill" Kill "Kill containers"
      <> dcParser "logs" Logs "View output from containers"
      <> dcParser "pause" Pause "Pause services"
      <> dcParser "port" Port "Print the public port for a port binding"
      <> dcParser "ps" Ps "List containers"
      <> dcParser "pull" Pull "Pull service images"
      <> dcParser "push" Push "Push service images"
      <> dcParser "restart" Restart "Restart services"
      <> dcParser "rm" Rm "Remove stopped containers"
      <> dcParser "run" Run "Run a one-off command"
      <> dcParser "scale" Scale "Set number of containers for a service"
      <> dcParser "start" Start "Start services"
      <> dcParser "stop" Stop "services"
      <> dcParser "top" Top "Display the running processes"
      <> dcParser "unpause" Unpause "Unpause services"
      <> dcParser "up" Up "Create and start containers"
      <> dcParser "version" Version "Show the Docker-Compose version information"

      <> metavar "DOCKER-COMPOSE-COMMAND"
      <> commandGroup "Docker Compose Commands:"
    )


dcParser
  :: Text
  -> DockerComposeCmd
  -> Text
  -> Mod CommandFields (CommonOptions -> IO ())
dcParser cmdStr cmd help =
  command
    (T.unpack cmdStr)
    (info
      (runDockerCompose <$> pure cmd <*> parseDockerComposeArgs)
      (progDesc (T.unpack help) <> fullDesc <> forwardOptions))


parseAll :: Parser (IO ())
parseAll =
  flip ($) <$> parseOptions <*> parseCommand


parseDockerComposeArgs :: Parser DockerComposeArgs
parseDockerComposeArgs =
  DockerComposeArgs <$>
    many (argument (T.pack <$> str) (metavar "DOCKER-COMPOSE ARGS..."))


shouldEval :: DockerComposeCmd -> Bool
shouldEval Up = True
shouldEval Down = True


runDockerCompose :: DockerComposeCmd -> DockerComposeArgs -> CommonOptions -> IO ()
runDockerCompose cmd args opts = T.putStrLn (show cmd)


runCat :: CommonOptions -> IO ()
runCat (CommonOptions files pkgs) = do
  T.putStrLn "Running cat ... TODO"
  T.putStrLn (modulesNixExpr files)


runRepl :: CommonOptions -> IO ()
runRepl opts = T.putStrLn "Running repl ... TODO"


runExec :: CommonOptions -> IO ()
runExec opts = T.putStrLn "Running exec ... TODO"


modulesNixExpr :: NonEmpty FilePath -> Text
modulesNixExpr =
        NE.toList
    >>> fmap pathExpr
    >>> T.unwords
    >>> wrapList
  where
    pathExpr path | isAbsolute path = "(/. + \""  <> T.pack path <> "\")"
                  | otherwise       = "(./. + \"" <> T.pack path <> "\")"

    isAbsolute ('/':_) = True
    isAbsolute _       = False

    wrapList s = "[ " <> s <> " ]"


main :: IO ()
main = 
  (join . execParser) (info (parseAll <**> helper) fullDesc)

