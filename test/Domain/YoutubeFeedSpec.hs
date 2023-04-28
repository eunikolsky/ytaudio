module Domain.YoutubeFeedSpec (spec) where

import Domain.YoutubeFeed
import Test.Hspec

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses something" $ do
      parse `shouldBe` ()
