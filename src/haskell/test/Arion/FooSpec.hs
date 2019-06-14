module Arion.FooSpec
  ( spec
  )
where

import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  it "foo" $ property True
