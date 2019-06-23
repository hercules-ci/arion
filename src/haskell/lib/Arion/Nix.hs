{-# LANGUAGE OverloadedStrings #-}
module Arion.Nix where

import           Prelude                        ( )
import           Protolude
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

data EvaluationArgs = EvaluationArgs
 { evalUid :: Int
 , evalModules :: NonEmpty FilePath
 , evalPkgs :: Text
 , evalWorkDir :: Maybe FilePath
 }

evaluate :: EvaluationArgs -> IO Value
evaluate ea = do
  evalComposition <- getDataFileName "nix/eval-composition.nix"
  let args =
        [ evalComposition
        , "--eval"
        , "--strict"
        , "--read-write-mode"
        , "--json"
        , "--show-trace"
        , "--argstr"
        , "uid"
        , show $ evalUid ea
        , "--arg"
        , "modules"
        , modulesNixExpr $ evalModules ea
        , "--arg"
        , "pkgs"
        , toS $ evalPkgs ea
        , "--attr"
        , "config.build.dockerComposeYamlAttrs"
        ]
      stdin    = mempty
      procSpec = (proc "nix-instantiate" args) { cwd = evalWorkDir ea }
  
  -- TODO: lazy IO is tricky. Let's use conduit/pipes instead?
  (exitCode, out, err) <- PBL.readCreateProcessWithExitCode procSpec stdin

  -- Stream 'err'
  errDone <- async (BL.hPutStr stderr err)

  -- Force 'out'
  v <- Protolude.evaluate (eitherDecode out)

  -- Wait for process exit and 'err' printout
  wait errDone

  case exitCode of
    ExitSuccess -> pass
    ExitFailure e -> throwIO $ FatalError "Evaluation failed" -- TODO: don't print this exception in main

  case v of
    Right r -> pure r
    Left  e -> throwIO $ FatalError "Couldn't parse nix-instantiate output"


modulesNixExpr :: NonEmpty FilePath -> [Char]
modulesNixExpr =
  NE.toList >>> fmap pathExpr >>> Data.String.unwords >>> wrapList
 where
  pathExpr :: FilePath -> [Char]
  pathExpr path | isAbsolute path = "(/. + \"/${" <> toNixStringLiteral path <> "}\")"
                | otherwise       = "(./. + \"/${" <> toNixStringLiteral path <> "}\")"

  isAbsolute ('/' : _) = True
  isAbsolute _         = False

  wrapList s = "[ " <> s <> " ]"

toNixStringLiteral :: [Char] -> [Char]
toNixStringLiteral = show -- FIXME: custom escaping including '$'
