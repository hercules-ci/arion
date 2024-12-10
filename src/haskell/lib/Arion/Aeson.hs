module Arion.Aeson where

import Data.Aeson
import Data.Aeson.Encode.Pretty
  ( confCompare,
    confTrailingNewline,
    defConfig,
  )
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Protolude
import Prelude ()

pretty :: (ToJSON a) => a -> Text
pretty =
  TL.toStrict
    . TB.toLazyText
    . Data.Aeson.Encode.Pretty.encodePrettyToTextBuilder' config
  where
    config = defConfig {confCompare = compare, confTrailingNewline = True}

decodeFile :: (FromJSON a) => FilePath -> IO a
decodeFile fp = do
  b <- BL.readFile fp
  case eitherDecode b of
    Left e -> panic (toS e)
    Right v -> pure v
