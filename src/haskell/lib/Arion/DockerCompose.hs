{-# LANGUAGE OverloadedStrings #-}
module Arion.DockerCompose where

import           Prelude                        ( )
import           Protolude
import           System.Process

data Args = Args
  { files :: [FilePath]
  , otherArgs :: [Text]
  , useSwarm :: Bool
  }

run :: Args -> IO ()
run args = do
  let (executable, fileParam) = case useSwarm args of
        False -> ("docker-compose", "--file")
        True -> ("docker", "--compose-file")
      fileArgs = files args >>= \f -> [fileParam, f]
      allArgs = case useSwarm args of
            False -> fileArgs ++ map toS (otherArgs args)
            True -> ["stack", "deploy"] ++ fileArgs ++ map toS (otherArgs args)

      procSpec = proc executable allArgs

  -- hPutStrLn stderr ("Running docker-compose with " <> show allArgs :: Text)

  withCreateProcess procSpec $ \_in _out _err procHandle -> do

    exitCode <- waitForProcess procHandle

    case exitCode of
      ExitSuccess -> pass
      ExitFailure 1 -> exitFailure
      ExitFailure {} -> do
        throwIO $ FatalError $ "docker-compose failed with " <> show exitCode
