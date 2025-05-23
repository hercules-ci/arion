module Arion.Nix
  ( evaluateComposition,
    withEvaluatedComposition,
    buildComposition,
    withBuiltComposition,
    replForComposition,
    EvaluationArgs (..),
    EvaluationMode (..),
  )
where

import Arion.Aeson (pretty)
import Control.Arrow ((>>>))
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.String
import qualified Data.Text.IO as T
import Paths_arion_compose
import Protolude
import qualified System.Directory as Directory
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import System.Process
import Prelude ()

data EvaluationMode
  = ReadWrite
  | ReadOnly

data EvaluationArgs = EvaluationArgs
  { posixUID :: Int,
    evalModulesFile :: NonEmpty FilePath,
    pkgsExpr :: Text,
    workDir :: Maybe FilePath,
    mode :: EvaluationMode,
    extraNixArgs :: [Text]
  }

evaluateComposition :: EvaluationArgs -> IO Value
evaluateComposition ea = do
  evalComposition <- getEvalCompositionFile
  let commandArgs =
        [ "--eval",
          "--strict",
          "--json",
          "--attr",
          "config.out.dockerComposeYamlAttrs"
        ]
      args =
        [evalComposition]
          ++ commandArgs
          ++ modeArguments ea.mode
          ++ argArgs ea
          ++ map toS ea.extraNixArgs
      procSpec =
        (proc "nix-instantiate" args)
          { cwd = ea.workDir,
            std_out = CreatePipe
          }

  withCreateProcess procSpec $ \_in outHM _err procHandle -> do
    let outHandle = fromMaybe (panic "stdout missing") outHM

    out <- BL.hGetContents outHandle

    v <- Protolude.evaluate (eitherDecode out)

    exitCode <- waitForProcess procHandle

    case exitCode of
      ExitSuccess -> pass
      ExitFailure 1 -> exitFailure
      ExitFailure {} -> do
        throwIO $ FatalError $ "evaluation failed with " <> show exitCode

    case v of
      Right r -> pure r
      Left e -> throwIO $ FatalError ("Couldn't parse nix-instantiate output" <> show e)

-- | Run with docker-compose.yaml tmpfile
withEvaluatedComposition :: EvaluationArgs -> (FilePath -> IO r) -> IO r
withEvaluatedComposition ea f = do
  v <- evaluateComposition ea
  withTempFile "." ".tmp-arion-docker-compose.yaml" $ \path yamlHandle -> do
    T.hPutStrLn yamlHandle (pretty v)
    hClose yamlHandle
    f path

buildComposition :: FilePath -> EvaluationArgs -> IO ()
buildComposition outLink ea = do
  evalComposition <- getEvalCompositionFile
  let commandArgs =
        [ "--attr",
          "config.out.dockerComposeYaml",
          "--out-link",
          outLink
        ]
      args =
        [evalComposition]
          ++ commandArgs
          ++ argArgs ea
          ++ map toS ea.extraNixArgs
      procSpec = (proc "nix-build" args) {cwd = ea.workDir}

  withCreateProcess procSpec $ \_in _out _err procHandle -> do
    exitCode <- waitForProcess procHandle

    case exitCode of
      ExitSuccess -> pass
      ExitFailure 1 -> exitFailure
      ExitFailure {} -> do
        throwIO $ FatalError $ "nix-build failed with " <> show exitCode

-- | Do something with a docker-compose.yaml.
withBuiltComposition :: EvaluationArgs -> (FilePath -> IO r) -> IO r
withBuiltComposition ea f = do
  withTempFile "." ".tmp-arion-docker-compose.yaml" $ \path emptyYamlHandle -> do
    hClose emptyYamlHandle
    -- Known problem: kills atomicity of withTempFile; won't fix because we should manage gc roots,
    -- impl of which will probably avoid this "problem". It seems unlikely to cause issues.
    Directory.removeFile path
    buildComposition path ea
    f path

replForComposition :: EvaluationArgs -> IO ()
replForComposition ea = do
  evalComposition <- getEvalCompositionFile
  let args =
        ["repl", "--file", evalComposition]
          ++ argArgs ea
          ++ map toS ea.extraNixArgs
      procSpec = (proc "nix" args) {cwd = ea.workDir}

  withCreateProcess procSpec $ \_in _out _err procHandle -> do
    exitCode <- waitForProcess procHandle

    case exitCode of
      ExitSuccess -> pass
      ExitFailure 1 -> exitFailure
      ExitFailure {} -> do
        throwIO $ FatalError $ "nix repl failed with " <> show exitCode

argArgs :: EvaluationArgs -> [[Char]]
argArgs ea =
  [ "--argstr",
    "uid",
    show ea.posixUID,
    "--arg",
    "modules",
    modulesNixExpr ea.evalModulesFile,
    "--arg",
    "pkgs",
    toS ea.pkgsExpr
  ]

getEvalCompositionFile :: IO FilePath
getEvalCompositionFile = getDataFileName "nix/eval-composition.nix"

modeArguments :: EvaluationMode -> [[Char]]
modeArguments ReadWrite = ["--read-write-mode"]
modeArguments ReadOnly = ["--readonly-mode"]

modulesNixExpr :: NonEmpty FilePath -> [Char]
modulesNixExpr =
  NE.toList >>> fmap pathExpr >>> Data.String.unwords >>> wrapList
  where
    pathExpr :: FilePath -> [Char]
    pathExpr path
      | isAbsolute path = "(/. + \"/${" <> toNixStringLiteral path <> "}\")"
      | otherwise = "(./. + \"/${" <> toNixStringLiteral path <> "}\")"

    isAbsolute ('/' : _) = True
    isAbsolute _ = False

    wrapList s = "[ " <> s <> " ]"

toNixStringLiteral :: [Char] -> [Char]
toNixStringLiteral = show -- FIXME: custom escaping including '$'
