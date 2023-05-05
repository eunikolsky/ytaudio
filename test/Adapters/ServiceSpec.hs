module Adapters.ServiceSpec (spec) where

import Data.ByteString.Lazy qualified as BSL
import Lib (app)
import Test.Hspec
import Test.Hspec.Wai

spec :: Spec
spec = with (return app) $ do
  describe "GET /feed/<channelid>" $ do
    it "responds with audio RSS" $ do
      get "/feed/<channelid>" `shouldRespondWith` nonEmptyBody

nonEmptyBody :: ResponseMatcher
nonEmptyBody =
  ResponseMatcher
    { matchBody = MatchBody $ \_headers body -> if BSL.null body then Just "received empty body" else Nothing
    , matchStatus = 200
    , matchHeaders = []
    }
