{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-

Parses the x-arion field in the generated compose file.

-}
module Arion.ExtendedInfo where

import Prelude()
import Protolude
import Data.Aeson as Aeson
import Arion.Aeson
import Control.Lens
import Data.Aeson.Lens

data Image = Image
  { image :: Maybe Text -- ^ image tar.gz file path
  , imageExe :: Maybe Text -- ^ path to exe producing image tar
  , imageName :: Text
  , imageTag :: Text
  } deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data ExtendedInfo = ExtendedInfo {
    projectName :: Maybe Text,
    images :: [Image]
  } deriving (Eq, Show)

loadExtendedInfoFromPath :: FilePath -> IO ExtendedInfo
loadExtendedInfoFromPath fp = do
  v <- decodeFile fp
  pure ExtendedInfo {
    -- TODO: use aeson derived instance?
    projectName = v ^? key "x-arion" . key "project" . key "name" . _String,
    images = (v :: Aeson.Value) ^.. key "x-arion" . key "images" . _Array . traverse . _JSON
  }
