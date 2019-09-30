{-# LANGUAGE OverloadedStrings #-}
module Arion.Nix
  ( evaluateComposition
  , withEvaluatedComposition
  , buildComposition
  , withBuiltComposition
  , replForComposition
  , EvaluationArgs(..)
  , EvaluationMode(..)
  ) where

import           Prelude                        ( )
import           Protolude
import           Arion.Aeson                    ( pretty )
import           Data.Aeson
import qualified Data.String
import qualified System.Directory              as Directory
import           System.Process
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import           Paths_arion_compose
import           Control.Applicative

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty             ( NonEmpty(..) )

import           Control.Arrow                  ( (>>>) )
import           System.IO.Temp                 ( withTempFile )
import           System.IO                      ( hClose )

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

evaluateComposition :: EvaluationArgs -> IO Value
evaluateComposition ea = do
  evalComposition <- getEvalCompositionFile
  let commandArgs =
        [ "--eval"
        , "--strict"
        , "--json"
        , "--attr"
        , "config.build.dockerComposeYamlAttrs"
        ]
      args =
        [ evalComposition ]
        ++ commandArgs
        ++ modeArguments (evalMode ea)
        ++ argArgs ea
        ++ map toS (evalUserArgs ea)
      procSpec = (proc "nix-instantiate" args)
        { cwd = evalWorkDir ea
        , std_out = CreatePipe
        }
  
  withCreateProcess procSpec $ \_in outHM _err procHandle -> do
    let outHandle = fromMaybe (panic "stdout missing") outHM

    out <- BL.hGetContents outHandle

    v <- Protolude.evaluate (eitherDecode out)

    exitCode <- waitForProcess procHandle

    case exitCode of
      ExitSuccess -> pass
      ExitFailure 1 -> exitFailure
      e@ExitFailure {} -> do
        throwIO $ FatalError $ "evaluation failed with " <> show exitCode
        exitWith e

    case v of
      Right r -> pure r
      Left  e -> throwIO $ FatalError "Couldn't parse nix-instantiate output"

-- | Run with docker-compose.yaml tmpfile
withEvaluatedComposition :: EvaluationArgs -> (FilePath -> IO r) -> IO r
withEvaluatedComposition ea f = do
  v <- evaluateComposition ea
  withTempFile "." ".tmp-arion-docker-compose.yaml" $ \path handle -> do
    T.hPutStrLn handle (pretty v)
    hClose handle
    f path


buildComposition :: FilePath -> EvaluationArgs -> IO ()
buildComposition outLink ea = do
  evalComposition <- getEvalCompositionFile
  let commandArgs =
        [ "--attr"
        , "config.build.dockerComposeYaml"
        , "--out-link"
        , outLink
        ]
      args =
        [ evalComposition ]
        ++ commandArgs
        ++ argArgs ea
        ++ map toS (evalUserArgs ea)
      procSpec = (proc "nix-build" args) { cwd = evalWorkDir ea }
  
  withCreateProcess procSpec $ \_in _out _err procHandle -> do

    exitCode <- waitForProcess procHandle

    case exitCode of
      ExitSuccess -> pass
      ExitFailure 1 -> exitFailure
      e@ExitFailure {} -> do
        throwIO $ FatalError $ "nix-build failed with " <> show exitCode
        exitWith e

-- | Do something with a docker-compose.yaml.
withBuiltComposition :: EvaluationArgs -> (FilePath -> IO r) -> IO r
withBuiltComposition ea f = do
  withTempFile "." ".tmp-arion-docker-compose.yaml" $ \path handle -> do
    hClose handle
    -- Known problem: kills atomicity of withTempFile; won't fix because we should manage gc roots,
    -- impl of which will probably avoid this "problem". It seems unlikely to cause issues.
    Directory.removeFile path
    buildComposition path ea
    f path


replForComposition :: EvaluationArgs -> IO ()
replForComposition ea = do
    evalComposition <- getEvalCompositionFile
    let args =
          [ "repl", evalComposition ]
          ++ argArgs ea
          ++ map toS (evalUserArgs ea)
        procSpec = (proc "nix" args) { cwd = evalWorkDir ea }
    
    withCreateProcess procSpec $ \_in _out _err procHandle -> do

      exitCode <- waitForProcess procHandle

      case exitCode of
        ExitSuccess -> pass
        ExitFailure 1 -> exitFailure
        e@ExitFailure {} -> do
          throwIO $ FatalError $ "nix repl failed with " <> show exitCode
          exitWith e

argArgs :: EvaluationArgs -> [[Char]]
argArgs ea =
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

getEvalCompositionFile :: IO FilePath
getEvalCompositionFile = getDataFileName "nix/eval-composition.nix"

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
