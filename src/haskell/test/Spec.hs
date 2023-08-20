module Spec
  ( spec
  )
where

import           Test.Hspec
import qualified Arion.NixSpec

spec :: Spec
spec = do
  describe "Arion.Nix" Arion.NixSpec.spec

