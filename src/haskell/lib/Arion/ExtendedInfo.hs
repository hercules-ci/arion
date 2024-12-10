{-

Parses the x-arion field in the generated compose file.

-}
module Arion.ExtendedInfo where

import Arion.Aeson
import Control.Lens
import Data.Aeson as Aeson
import Data.Aeson.Lens
import Protolude
import Prelude ()

data Image = Image
  { -- | image tar.gz file path
    image :: Maybe Text,
    -- | path to exe producing image tar
    imageExe :: Maybe Text,
    imageName :: Text,
    imageTag :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data ExtendedInfo = ExtendedInfo
  { projectName :: Maybe Text,
    images :: [Image]
  }
  deriving stock (Eq, Show)

loadExtendedInfoFromPath :: FilePath -> IO ExtendedInfo
loadExtendedInfoFromPath fp = do
  v <- decodeFile fp
  pure
    ExtendedInfo
      { -- TODO: use aeson derived instance?
        projectName = v ^? key "x-arion" . key "project" . key "name" . _String,
        images = (v :: Aeson.Value) ^.. key "x-arion" . key "images" . _Array . traverse . _JSON
      }
