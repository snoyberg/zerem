module ZeremSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Zerem as Z
import Zerem.Prelude

spec :: Spec
spec = do
  describe "summation" $ do
    it "sanity" $
      runIdentity (Z.sum (Z.enumFromTo 1 (10 :: Int))) `shouldBe` 55
    prop "matches lists" $ \x' y' -> do
      let (x, y) = if x' < y' then (x', y') else (y', x')
      sum [x..y :: Int] `shouldBe`
        runIdentity (Z.sum (Z.enumFromTo x y))
