module Usecases.AudioFeed
  ( downloadAudioFeed
  , ChannelId (..)
  )
where

-- `Domain` types are imported with the `Dom.` prefix, whereas functions and
-- field names are imported directly to reduce noise
import Data.Either
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Version
import Domain.AudioFeed hiding (AudioFeed)
import Domain.AudioFeed qualified as Dom
import Domain.AudioFeed.Item hiding (AudioFeedItem)
import Domain.AudioFeed.Item qualified as Dom
import Polysemy
import Text.RSS.Types
import URI.ByteString
import Usecases.Youtube

-- Audio feed on the `Usecases` layer is an `RssDocument'` because that's the
-- form that can be directly serialized to XML (with `rss-conduit`).
downloadAudioFeed
  :: (Member Youtube r) => (Text -> Maybe Dom.AudioFeed) -> ChannelId -> Sem r (Maybe RssDocument')
-- TODO should `parseFeed` be a separate effect, even though it's a pure function?
downloadAudioFeed parseFeed channelId = do
  feed <- getChannelFeed channelId
  let audioFeed = parseFeed feed
  pure $ mkRssDoc <$> audioFeed

{-
 - Note: `mkRssDoc` is a pure function, so I think it has its place in the
 - `Domain` layer. However, the result type is `RssDocument'` [1] from the
 - `rss-conduit` library, which is one of the libraries that can be used to
 - render RSS. It's placed in the `Usecases` layer to reduce the dependency
 - of the `Domain` layer on other libraries.
 -
 - [1]: `RssDocument'` is an intermediary step between the `Domain.AudioFeed`
 - (containing only the data that can be parsed from a Youtube feed) and the
 - resulting `ByteString` (containing the RSS XML). Testing the rendered XML
 - directly would be much more annoying.
 -}

mkRssDoc :: Dom.AudioFeed -> RssDocument'
mkRssDoc Dom.AudioFeed{afTitle, afLink, afItems} =
  RssDocument
    { -- fields dependent on `Dom.AudioFeed`
      channelTitle = afTitle
    , channelLink = urlToURI afLink
    , channelItems = mkRssItem <$> afItems
    , -- static fields
      documentVersion = makeVersion [2, 0]
    , -- unused fields
      channelDescription = ""
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
    }

mkRssItem :: Dom.AudioFeedItem -> RssItem'
mkRssItem Dom.AudioFeedItem{afiTitle, afiGuid, afiPubDate, afiDescription, afiLink} =
  RssItem
    { itemTitle = afiTitle
    , itemLink = Just . urlToURI $ afiLink
    , itemDescription = afiDescription
    , itemEnclosure
    , itemGuid = Just . GuidText . getYoutubeVideoId $ afiGuid
    , itemPubDate = Just afiPubDate
    , -- unused fields
      itemAuthor = ""
    , itemCategories = []
    , itemComments = Nothing
    , itemSource = Nothing
    , itemExtensions = NoItemExtensions
    }
  where
    itemEnclosure =
      [ RssEnclosure
          { enclosureUrl = urlToURI $ "http://localhost:3000/yt/" <> getYoutubeVideoId afiGuid
          , enclosureLength = unknownLength
          , enclosureType = "audio/mpeg"
          }
      ]

{- | Represents an unknown length of the audio file because the file will be
re-encoded and streamed directly. `enclosure.length` is required, and `0`
means an unknown length according to W3C's Feed Validator at
https://validator.w3.org/feed/.
-}
unknownLength :: (Num a) => a
unknownLength = 0

-- | Expects a valid (youtube) URL and returns it as the `RssURI` type.
urlToURI :: Text -> RssURI
urlToURI url = RssURI . fromRight err . parseURI strictURIParserOptions . TE.encodeUtf8 $ url
  where
    -- TODO propagate error to the caller?
    err = error $ "failed to parse URI " <> T.unpack url
