module Domain.AudioFeedSpec (spec) where

import Data.Map as M
import Data.Text (Text)
import Domain.AudioFeed
import Domain.AudioFeed.Item
import Domain.LiveStatus
import Domain.YoutubeVideoId (YoutubeVideoId (YoutubeVideoId))
import Domain.YtDlpChannelStreams
import Test.Hspec

spec :: Spec
spec = do
  describe "dropUnavailable" $ do
    it "filters out upcoming streams" $
      dropUnavailable streams feed `shouldBe` filteredFeed

    it "leaves videos not found in the streams" $ do
      let feed' = feed{afItems = [mkItemWithGuid "id3"]}
      dropUnavailable streams feed' `shouldBe` feed'

streams :: Streams
streams =
  Streams $
    M.fromList
      [ (YoutubeVideoId "id0", IsUpcoming)
      , (YoutubeVideoId "id1", WasLive)
      , (YoutubeVideoId "id2", IsUpcoming)
      ]

feed :: AudioFeed
feed =
  AudioFeed
    { afTitle = "title"
    , afLink = "link"
    , afItems = [mkItemWithGuid "id0", mkItemWithGuid "id1", mkItemWithGuid "id2"]
    }

filteredFeed :: AudioFeed
filteredFeed =
  feed
    { afItems = [mkItemWithGuid "id1"]
    }

mkItemWithGuid :: Text -> AudioFeedItem
mkItemWithGuid guid = AudioFeedItem ("title " <> guid) (YoutubeVideoId guid) (read "2023-01-01 00:00:00 UTC") "" "link"
