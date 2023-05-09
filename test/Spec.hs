module Main (main) where

import Adapters.ServiceSpec qualified (spec)
import Domain.LiveStatusSpec qualified (spec)
import Domain.YoutubeFeedSpec qualified (spec)
import Test.Hspec
import Usecases.AudioFeedSpec qualified (spec)

-- FIXME use hspec-discover
main :: IO ()
main = hspec $ do
  Domain.LiveStatusSpec.spec
  Domain.YoutubeFeedSpec.spec
  Usecases.AudioFeedSpec.spec
  Adapters.ServiceSpec.spec
