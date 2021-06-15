{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Arion.Images
  ( loadImages
  ) where

import Prelude()
import Protolude hiding (to)

import qualified System.Process as Process
import qualified Data.Text as T

import Arion.ExtendedInfo (Image(..))

type TaggedImage = Text

-- | Subject to change
loadImages :: Bool -> [Image] -> IO ()
loadImages isPodman requestedImages = do

  loaded <- getDockerImages isPodman

  let
    isNew i =
      -- On docker, the image name is unmodified
      (imageName i <> ":" <> imageTag i) `notElem` loaded
      -- -- On podman, you automatically get a localhost prefix
        && ("localhost/" <> imageName i <> ":" <> imageTag i) `notElem` loaded

  traverse_ (loadImage isPodman) . filter isNew $ requestedImages

exeName :: IsString p => Bool -> p
exeName _isPodman@True = "podman"
exeName _isPodman@False = "docker"

loadImage :: Bool -> Image -> IO ()
loadImage isPodman Image { image = Just imgPath, imageName = name } =
  withFile (toS imgPath) ReadMode $ \fileHandle -> do
  let procSpec = (Process.proc (exeName isPodman) [ "load" ]) {
          Process.std_in = Process.UseHandle fileHandle
        }
  print procSpec
  Process.withCreateProcess procSpec $ \_in _out _err procHandle -> do
    e <- Process.waitForProcess procHandle
    case e of
      ExitSuccess -> pass
      ExitFailure code ->
        panic $ exeName isPodman <> " load failed with exit code " <> show code <> " for image " <> name <> " from path " <> imgPath

loadImage isPodman Image { imageExe = Just imgExe, imageName = name } = do
  let loadSpec = (Process.proc (exeName isPodman) [ "load" ]) { Process.std_in = Process.CreatePipe }
  Process.withCreateProcess loadSpec $ \(Just inHandle) _out _err loadProcHandle -> do
    let streamSpec = Process.proc (toS imgExe) []
    Process.withCreateProcess streamSpec { Process.std_out = Process.UseHandle inHandle } $ \_ _ _ streamProcHandle ->
      withAsync (Process.waitForProcess loadProcHandle) $ \loadExitAsync ->
        withAsync (Process.waitForProcess streamProcHandle) $ \streamExitAsync -> do
          r <- waitEither loadExitAsync streamExitAsync
          case r of
            Right (ExitFailure code) -> panic $ "image producer for image " <> name <> " failed with exit code " <> show code <> " from executable " <> imgExe
            Right ExitSuccess -> pass
            Left _ -> pass
          loadExit <- wait loadExitAsync
          case loadExit of
            ExitFailure code -> panic $ exeName isPodman <> " load failed with exit code " <> show code <> " for image " <> name <> " produced by executable " <> imgExe
            _ -> pass
          pass

loadImage _isPodman Image { imageName = name } = do
  panic $ "image " <> name <> " doesn't specify an image file or imageExe executable"


getDockerImages :: Bool -> IO [TaggedImage]
getDockerImages isPodman = do
  let procSpec = Process.proc (exeName isPodman) [ "images",  "--filter", "dangling=false", "--format", "{{.Repository}}:{{.Tag}}" ]
  map toS . T.lines . toS <$> Process.readCreateProcess procSpec ""
