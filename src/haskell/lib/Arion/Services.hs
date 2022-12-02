{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Arion.Services
  ( getDefaultExec
  ) where

import Prelude()
import Protolude hiding (to)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AK
import           Arion.Aeson (decodeFile)

import Control.Lens
import Data.Aeson.Lens

-- | Subject to change
getDefaultExec :: FilePath -> Text -> IO [Text]
getDefaultExec fp service = do

  v <- decodeFile fp

  pure ((v :: Aeson.Value) ^.. key "x-arion" . key "serviceInfo" . key (AK.fromText service) . key "defaultExec" . _Array . traverse . _String)
