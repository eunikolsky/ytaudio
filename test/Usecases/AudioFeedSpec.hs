{-# LANGUAGE QuasiQuotes #-}

module Usecases.AudioFeedSpec (spec) where

import Data.Set qualified as S
import Data.Text (Text)
import Data.Time.Clock
import Data.Version (makeVersion)
import Domain.AudioFeed hiding (AudioFeed)
import Domain.AudioFeed qualified as Dom
import Domain.AudioFeed.Item hiding (AudioFeedItem)
import Domain.AudioFeed.Item qualified as Dom
import Polysemy
import Polysemy.Error
import Test.Hspec
import Text.RSS.Types
import URI.ByteString.QQ
import Usecases.AudioFeed qualified as UC
import Usecases.RunYoutubePure

spec :: Spec
spec = do
  describe "getAudioFeed" $ do
    it "creates RSS doc for youtube channel" $ do
      runDownloadAudioFeed audioFeed channelId `shouldBe` Right rssDoc

{- | Runs the `downloadAudioFeed` usecase purely, mocking the youtube downloader
to return an empty string and then using `audioFeed` as the parsed value.
-}
runDownloadAudioFeed
  :: Dom.AudioFeed -> UC.ChannelId -> Either UC.DownloadAudioFeedError RssDocument'
runDownloadAudioFeed feed =
  run
    . runError
    . runYoutubePure (channelId, testDownloadedText)
    . UC.downloadAudioFeed audioFeedParser
  where
    audioFeedParser text
      | text == testDownloadedText = Just feed
      | otherwise = Nothing

channelId :: UC.ChannelId
channelId = UC.ChannelId "UC5VtdI-WX"

{- | This text is used to ensure that whatever the `Youtube` effect returns for
the channel feed is what's passed to the audio feed parser.
-}
testDownloadedText :: Text
testDownloadedText = "downloaded text"

testPubDate :: UTCTime
testPubDate = read "2023-02-01 08:00:00 UTC"

audioFeed :: Dom.AudioFeed
audioFeed =
  Dom.AudioFeed
    { afTitle = "привет"
    , afLink = "http://youtube.com/user/123"
    , afItems =
        [ Dom.AudioFeedItem
            { afiTitle = "мир"
            , afiGuid = YoutubeVideoId "hC0de9"
            , afiPubDate = testPubDate
            , afiDescription = "описание\nздесь"
            , afiLink = "https://youtube.com/watch?v=hC0de9"
            }
        ]
    }

rssDoc :: RssDocument'
rssDoc =
  RssDocument
    { documentVersion = makeVersion [2, 0]
    , channelTitle = "привет"
    , channelLink = RssURI [uri|http://youtube.com/user/123|]
    , channelDescription = ""
    , channelLanguage = ""
    , channelCopyright = ""
    , channelManagingEditor = ""
    , channelWebmaster = ""
    , channelPubDate = Nothing
    , channelLastBuildDate = Nothing
    , channelCategories = []
    , channelGenerator = ""
    , channelDocs = Nothing
    , channelCloud = Nothing
    , channelTtl = Nothing
    , channelImage = Nothing
    , channelRating = ""
    , channelTextInput = Nothing
    , channelSkipHours = S.empty
    , channelSkipDays = S.empty
    , channelExtensions = NoChannelExtensions
    , channelItems =
        [ RssItem
            { itemTitle = "мир"
            , itemLink = Just . RssURI $ [uri|https://youtube.com/watch?v=hC0de9|]
            , itemDescription = "описание\nздесь"
            , itemAuthor = ""
            , itemCategories = []
            , itemComments = Nothing
            , itemEnclosure = [enclosure]
            , itemGuid = Just $ GuidText "hC0de9"
            , itemPubDate = Just testPubDate
            , itemSource = Nothing
            , itemExtensions = NoItemExtensions
            }
        ]
    }
  where
    enclosure =
      RssEnclosure
        { enclosureUrl = RssURI [uri|http://localhost:3000/yt/hC0de9|]
        , enclosureLength = 0
        , enclosureType = "audio/mpeg"
        }
