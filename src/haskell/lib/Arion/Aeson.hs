module Arion.Aeson where

import           Data.Aeson
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TL
import qualified Data.Text.Lazy.Builder        as TB
import qualified Data.Aeson.Encode.Pretty
import           Data.Aeson.Encode.Pretty       ( defConfig
                                                , keyOrder
                                                , confCompare
                                                , confTrailingNewline
                                                )
import           Protolude

pretty :: ToJSON a => a -> Text
pretty =
  TL.toStrict
    . TB.toLazyText
    . Data.Aeson.Encode.Pretty.encodePrettyToTextBuilder' config
  where config = defConfig { confCompare = compare, confTrailingNewline = True }
