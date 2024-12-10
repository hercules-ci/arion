module Spec
  ( spec,
  )
where

import qualified Arion.NixSpec
import Test.Hspec

spec :: Spec
spec = do
  describe "Arion.Nix" Arion.NixSpec.spec
