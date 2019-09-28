{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Arion.Services
  ( getDefaultExec
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

-- | Subject to change
getDefaultExec :: FilePath -> Text -> IO [Text]
getDefaultExec fp service = do

  v <- decodeFile fp

  pure ((v :: Aeson.Value) ^.. key "x-arion" . key "serviceInfo" . key service . key "defaultExec" . _Array . traverse . _String)
