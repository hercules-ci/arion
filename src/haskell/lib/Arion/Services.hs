{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Arion.Services
  ( getDefaultExec
  ) where

import Prelude()
import Protolude hiding (to)

import qualified Data.Aeson as Aeson
#if MIN_VERSION_lens_aeson(1,2,0)
import qualified Data.Aeson.Key as AK
#endif
import           Arion.Aeson (decodeFile)

import Control.Lens
import Data.Aeson.Lens

#if MIN_VERSION_lens_aeson(1,2,0)
type Key = AK.Key
mkKey :: Text -> Key
mkKey = AK.fromText
#else
type Key = Text
mkKey :: Text -> Key
mkKey = identity
#endif

-- | Subject to change
getDefaultExec :: FilePath -> Text -> IO [Text]
getDefaultExec fp service = do

  v <- decodeFile fp

  pure ((v :: Aeson.Value) ^.. key "x-arion" . key "serviceInfo" . key (mkKey service) . key "defaultExec" . _Array . traverse . _String)
