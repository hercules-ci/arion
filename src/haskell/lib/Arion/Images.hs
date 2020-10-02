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
  { image :: Text -- ^ file path
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

  traverse_ loadImage . map (toS . image) . filter isNew $ images

loadImage :: FilePath -> IO ()
loadImage imgPath = withFile (imgPath) ReadMode $ \fileHandle -> do
  let procSpec = (Process.proc "docker" [ "load" ]) {
          Process.std_in = Process.UseHandle fileHandle
        }
  Process.withCreateProcess procSpec $ \_in _out _err procHandle -> do
    e <- Process.waitForProcess procHandle 
    case e of
      ExitSuccess -> pass
      ExitFailure code -> panic $ "docker load (" <> show code <> ") failed for " <> toS imgPath


getDockerImages :: IO [TaggedImage]
getDockerImages = do
  let procSpec = Process.proc "docker" [ "images",  "--filter", "dangling=false", "--format", "{{.Repository}}:{{.Tag}}" ]
  (map toS . T.lines . toS) <$> Process.readCreateProcess procSpec ""
