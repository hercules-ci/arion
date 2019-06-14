module Spec
  ( spec
  )
where

import           Test.Hspec
import qualified Arion.FooSpec

spec :: Spec
spec = do
  describe "Arion.Foo" Arion.FooSpec.spec
