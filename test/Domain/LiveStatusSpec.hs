module Domain.LiveStatusSpec (spec) where

import Control.Monad
import Domain.LiveStatus
import Test.Hspec

spec :: Spec
spec = do
  describe "LiveStatus" $ do
    it "can be constructed from a string" $ do
      let mapping =
            [ ("not_live", NotLive)
            , ("is_live", IsLive)
            , ("is_upcoming", IsUpcoming)
            , ("was_live", WasLive)
            , ("post_live", PostLive)
            ]

      forM_ mapping $ \(str, expected) ->
        liveStatusFromString str `shouldBe` Just expected

    it "returns Nothing for unknown strings" $ do
      liveStatusFromString "unknown" `shouldBe` Nothing
