{-# LANGUAGE OverloadedStrings #-}
module Arion.DockerCompose where

import           Prelude                        ( )
import           Protolude
import           System.Process

data Args = Args
  { files :: [FilePath]
  , otherArgs :: [Text]
  , isPodman :: Bool
  }

run :: Args -> IO ()
run args = do
  let fileArgs = files args >>= \f -> ["--file", f]
      allArgs  = fileArgs ++ map toS (otherArgs args)

      exeName = if isPodman args then "podman-compose" else "docker-compose"
      procSpec = 
        proc exeName allArgs

  withCreateProcess procSpec $ \_in _out _err procHandle -> do

    exitCode <- waitForProcess procHandle

    case exitCode of
      ExitSuccess -> pass
      ExitFailure 1 -> exitFailure
      ExitFailure {} -> do
        throwIO $ FatalError $ toS exeName <> " failed with status " <> show exitCode
