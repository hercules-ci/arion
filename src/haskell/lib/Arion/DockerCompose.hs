{-# LANGUAGE OverloadedStrings #-}
module Arion.DockerCompose where

import           Prelude                        ( )
import           Protolude
import           Arion.Aeson                    ( pretty )
import           Data.Aeson
import qualified Data.String
import           System.Process
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified System.Process.ByteString.Lazy
                                                as PBL
import           Paths_arion_compose
import           Control.Applicative

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty             ( NonEmpty(..) )

import           Control.Arrow                  ( (>>>) )
import           System.IO.Temp                 ( withTempFile )
import           System.IO                      ( hClose )

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

    -- Wait for process exit and 'err' printout
    exitCode <- waitForProcess procHandle

    case exitCode of
      ExitSuccess -> pass
      ExitFailure 1 -> exitFailure
      e@ExitFailure {} -> do
        throwIO $ FatalError $ "docker-compose failed with " <> show exitCode
        exitWith e
