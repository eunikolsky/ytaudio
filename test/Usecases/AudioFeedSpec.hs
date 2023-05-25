{-# LANGUAGE QuasiQuotes #-}

module Usecases.AudioFeedSpec (spec) where

import Data.Functor
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Time.Clock
import Data.Version (makeVersion)
import Domain.AudioFeed hiding (AudioFeed)
import Domain.AudioFeed qualified as Dom
import Domain.AudioFeed.Item hiding (AudioFeedItem)
import Domain.AudioFeed.Item qualified as Dom
import Domain.LiveStatus qualified as Dom
import Domain.YoutubeVideoId qualified as Dom
import Domain.YtDlpChannelStreams qualified as Dom
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Test.Hspec
import Text.RSS.Types
import URI.ByteString (Port (..))
import URI.ByteString.QQ
import Usecases.AudioFeed qualified as UC
import Usecases.RunYoutubePure

spec :: Spec
spec = do
  describe "getAudioFeed" $ do
    it "creates RSS doc for youtube channel" $ do
      pendingWith "FIXME downloadAudioFeed's type has changed"
      let streams = Dom.Streams mempty
      runDownloadAudioFeed (audioFeed []) streams channelId `shouldBe` Right rssDoc

    it "filters out upcoming streams" $ do
      pendingWith "FIXME downloadAudioFeed's type has changed"
      let streams =
            Dom.Streams $
              M.fromList
                [ (Dom.YoutubeVideoId "hC0de9", Dom.WasLive)
                , (Dom.YoutubeVideoId "foo", Dom.IsUpcoming)
                , (Dom.YoutubeVideoId "bar", Dom.IsUpcoming)
                ]
      runDownloadAudioFeed
        (audioFeed [Dom.YoutubeVideoId "foo", Dom.YoutubeVideoId "bar"])
        streams
        channelId
        `shouldBe` Right rssDoc

{- | Runs the `downloadAudioFeed` usecase purely, mocking the youtube downloader
to return an empty string and then using `audioFeed` as the parsed value.
-}
runDownloadAudioFeed
  :: Dom.AudioFeed -> Dom.Streams -> UC.ChannelId -> Either UC.DownloadAudioFeedError RssDocument'
runDownloadAudioFeed feed streams =
  undefined
    run
    . runInputConst (Port 8080)
    . runError
    . runYoutubePure (channelId, testDownloadedText, streams)
    . undefined -- UC.downloadAudioFeed audioFeedParser
  where
    _audioFeedParser text
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

audioFeed :: [Dom.YoutubeVideoId] -> Dom.AudioFeed
audioFeed extraItemGuids =
  Dom.AudioFeed
    { afTitle = "привет"
    , afLink = "http://youtube.com/user/123"
    , afItems
    }
  where
    afItems =
      itemGuids <&> \afiGuid ->
        Dom.AudioFeedItem
          { afiTitle = "мир"
          , afiGuid
          , afiPubDate = testPubDate
          , afiDescription = "описание\nздесь"
          , afiLink = "https://youtube.com/watch?v=hC0de9"
          }

    itemGuids = Dom.YoutubeVideoId "hC0de9" : extraItemGuids

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
        { enclosureUrl = RssURI [uri|http://localhost:8080/yt/hC0de9|]
        , enclosureLength = 0
        , enclosureType = "audio/mpeg"
        }
