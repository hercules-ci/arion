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
import qualified Data.ByteString as BS
import qualified System.Process as Process

import Control.Lens
import Data.Aeson.Lens
import Data.String
import System.IO (withFile, IOMode(ReadMode))


data Image = Image
  { image :: Text -- ^ file path
  , imageName :: Text
  , imageTag :: Text
  } deriving (Generic, Aeson.ToJSON, Aeson.FromJSON, Show)

type TaggedImage = Text

loadImages :: FilePath -> IO ()
loadImages fp = do

  v <- decodeFile fp

  loaded <- dockerImages

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


dockerImages :: IO [TaggedImage]
dockerImages = do
  let procSpec = Process.proc "docker" [ "images",  "--filter", "dangling=false", "--format", "{{.Repository}}:{{.Tag}}" ]
  (map toS . lines) <$> Process.readCreateProcess procSpec ""
