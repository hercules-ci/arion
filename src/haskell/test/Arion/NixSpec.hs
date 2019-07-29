{-# LANGUAGE OverloadedStrings #-}
module Arion.NixSpec
  ( spec
  )
where

import           Protolude
import           Test.Hspec
import           Test.QuickCheck
import qualified Data.List.NonEmpty            as NEL
import           Arion.Aeson
import           Arion.Nix
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy.IO             as TL
import qualified Data.Text.Lazy.Builder        as TB
import qualified Data.Aeson.Encode.Pretty
import Data.Char (isSpace)

spec :: Spec
spec = describe "evaluate" $ it "matches an example" $ do
  x <- Arion.Nix.evaluate EvaluationArgs
    { evalUid      = 123
    , evalModules  = NEL.fromList
                       ["src/haskell/testdata/Arion/NixSpec/arion-compose.nix"]
    , evalPkgs     = "import <nixpkgs> {}"
    , evalWorkDir  = Nothing
    , evalMode     = ReadOnly
    , evalUserArgs = ["--show-trace"]
    }
  let actual = pretty x
  expected <- T.readFile "src/haskell/testdata/Arion/NixSpec/arion-compose.json"
  censorPaths actual `shouldBe` censorPaths expected

censorPaths :: Text -> Text
censorPaths x = case T.breakOn "/nix/store/" x of
  (prefix, tl) | (tl :: Text) == "" -> prefix
  (prefix, tl) -> prefix <> "<STOREPATH>" <> censorPaths
    (T.dropWhile isNixNameChar $ T.drop (T.length "/nix/store/") tl)

-- | WARNING: THIS IS LIKELY WRONG: DON'T REUSE
isNixNameChar :: Char -> Bool
isNixNameChar c | c >= '0' && c <= '9' = True
isNixNameChar c | c >= 'a' && c <= 'z' = True
isNixNameChar c | c >= 'A' && c <= 'Z' = True
isNixNameChar c | c == '-' = True
isNixNameChar c | c == '.' = True
isNixNameChar c | c == '_' = True -- WRONG?
isNixNameChar c = False -- WRONG?
