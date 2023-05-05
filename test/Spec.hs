module Main (main) where

import Adapters.ServiceSpec qualified (spec)
import Domain.YoutubeFeedSpec qualified (spec)
import Test.Hspec
import Usecases.AudioFeedSpec qualified (spec)

main :: IO ()
main = hspec $ do
  Domain.YoutubeFeedSpec.spec
  Usecases.AudioFeedSpec.spec
  Adapters.ServiceSpec.spec
