module Main (main) where

import Domain.YoutubeFeedSpec qualified (spec)
import Lib (app)
import Test.Hspec
import Test.Hspec.Wai

main :: IO ()
main = hspec $ do
  Domain.YoutubeFeedSpec.spec
  spec

spec :: Spec
spec = with (return app) $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200

    it "responds with hello world" $ do
      get "/" `shouldRespondWith` "hello world"
