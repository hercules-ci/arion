{-# LANGUAGE OverloadedStrings #-}
module Arion.DockerCompose where

import           Prelude                        ( )
import           Protolude
import           System.Process

data Args = Args
  { files :: [FilePath]
  , otherArgs :: [Text]
  }

run :: Args -> IO ()
run args = do
  let fileArgs = files args >>= \f -> ["--file", f]
      allArgs  = fileArgs ++ map toS (otherArgs args)

      procSpec = proc "docker-compose" allArgs

  -- hPutStrLn stderr ("Running docker-compose with " <> show allArgs :: Text)

  withCreateProcess procSpec $ \_in _out _err procHandle -> do

    exitCode <- waitForProcess procHandle

    case exitCode of
      ExitSuccess -> pass
      ExitFailure 1 -> exitFailure
      ExitFailure {} -> do
        throwIO $ FatalError $ "docker-compose failed with " <> show exitCode
