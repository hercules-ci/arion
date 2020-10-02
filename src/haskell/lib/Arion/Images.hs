{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Arion.Images 
  ( loadImages
  ) where

import Prelude()
import Protolude hiding (to)

import qualified Data.Aeson as Aeson
import           Arion.Aeson (decodeFile)
import qualified System.Process as Process
import qualified Data.Text as T

import Control.Lens
import Data.Aeson.Lens

data Image = Image
  { image :: Maybe Text -- ^ image tar.gz file path
  , imageExe :: Maybe Text -- ^ path to exe producing image tar
  , imageName :: Text
  , imageTag :: Text
  } deriving (Generic, Aeson.ToJSON, Aeson.FromJSON, Show)

type TaggedImage = Text

-- | Subject to change
loadImages :: FilePath -> IO ()
loadImages fp = do

  v <- decodeFile fp

  loaded <- getDockerImages

  let
    images :: [Image]
    images = (v :: Aeson.Value) ^.. key "x-arion" . key "images" . _Array . traverse . _JSON

    isNew i = (imageName i <> ":" <> imageTag i) `notElem` loaded

  traverse_ loadImage . filter isNew $ images

loadImage :: Image -> IO ()
loadImage (Image { image = Just imgPath, imageName = name }) =
  withFile (toS imgPath) ReadMode $ \fileHandle -> do
  let procSpec = (Process.proc "docker" [ "load" ]) {
          Process.std_in = Process.UseHandle fileHandle
        }
  Process.withCreateProcess procSpec $ \_in _out _err procHandle -> do
    e <- Process.waitForProcess procHandle 
    case e of
      ExitSuccess -> pass
      ExitFailure code ->
        panic $ "docker load failed with exit code " <> show code <> " for image " <> name <> " from path " <> imgPath

loadImage (Image { imageExe = Just imgExe, imageName = name }) = do
  let loadSpec = (Process.proc "docker" [ "load" ]) { Process.std_in = Process.CreatePipe }
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
            ExitFailure code -> panic $ "docker load failed with exit code " <> show code <> " for image " <> name <> " produced by executable " <> imgExe
            _ -> pass
          pass

loadImage (Image { imageName = name }) = do
  panic $ "image " <> name <> " doesn't specify an image file or imageExe executable"


getDockerImages :: IO [TaggedImage]
getDockerImages = do
  let procSpec = Process.proc "docker" [ "images",  "--filter", "dangling=false", "--format", "{{.Repository}}:{{.Tag}}" ]
  (map toS . T.lines . toS) <$> Process.readCreateProcess procSpec ""
