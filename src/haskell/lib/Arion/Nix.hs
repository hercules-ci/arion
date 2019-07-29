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

data EvaluationMode =
  ReadWrite | ReadOnly

data EvaluationArgs = EvaluationArgs
 { evalUid :: Int
 , evalModules :: NonEmpty FilePath
 , evalPkgs :: Text
 , evalWorkDir :: Maybe FilePath
 , evalMode :: EvaluationMode
 , evalUserArgs :: [Text]
 }

evaluate :: EvaluationArgs -> IO Value
evaluate ea = do
  evalComposition <- getDataFileName "nix/eval-composition.nix"
  let commandArgs =
        [ "--eval"
        , "--strict"
        , "--json"
        , "--attr"
        , "config.build.dockerComposeYamlAttrs"
        ]
      argArgs =
        [ "--argstr"
        , "uid"
        , show $ evalUid ea
        , "--arg"
        , "modules"
        , modulesNixExpr $ evalModules ea
        , "--arg"
        , "pkgs"
        , toS $ evalPkgs ea
        ]
      args =
        [ evalComposition ]
        ++ commandArgs
        ++ modeArguments (evalMode ea)
        ++ argArgs
        ++ map toS (evalUserArgs ea)
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

modeArguments :: EvaluationMode -> [[Char]]
modeArguments ReadWrite = [ "--read-write-mode" ]
modeArguments ReadOnly = [ "--readonly-mode" ]

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
