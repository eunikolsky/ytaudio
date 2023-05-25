module Usecases.AudioFeed
  ( downloadAudioFeed
  , DownloadAudioFeedError (..)
  , ChannelId (..)
  )
where

-- `Domain` types are imported with the `Dom.` prefix, whereas functions and
-- field names are imported directly to reduce noise

import Conduit
import Data.Binary.Builder qualified as BB
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
import Domain.YoutubeVideoId hiding (YoutubeVideoId)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Text.RSS.Conduit.Render
import Text.RSS.Types
import Text.XML.Stream.Render
import URI.ByteString
import Usecases.Youtube

-- | Errors that can happen in the `downloadAudioFeed` usecase.
newtype DownloadAudioFeedError = YoutubeFeedParseError Text
  deriving stock (Show, Eq)

-- Audio feed on the `Usecases` layer is a conduit of byte strings (builders)
-- because all the feed's items may not be available immediately and we need to
-- stream them as they become available.
downloadAudioFeed
  -- TODO `Port` comes from `uri-bytestring` — is this fine?
  :: (Members [Youtube, Error DownloadAudioFeedError, Input Port] r, Monad m)
  => (Text -> Maybe Dom.AudioFeed)
  -> ChannelId
  -> Sem r (ConduitT () BB.Builder m ())
-- TODO should `parseFeed` be a separate effect, even though it's a pure function?
downloadAudioFeed parseFeed channelId = do
  feed <- getChannelFeed channelId
  case parseFeed feed of
    Just audioFeed -> do
      -- TODO download these in parallel
      streams <- getChannelStreams channelId
      let downloadableAudioFeed = Dom.dropUnavailable streams audioFeed
      doc <- mkRssDoc downloadableAudioFeed
      pure $ renderRssDocument doc .| renderBuilder def
    Nothing -> throw $ YoutubeFeedParseError feed

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

mkRssDoc :: (Member (Input Port) r) => Dom.AudioFeed -> Sem r RssDocument'
mkRssDoc Dom.AudioFeed{afTitle, afLink, afItems} = do
  channelItems <- traverse mkRssItem afItems
  pure
    RssDocument
      { -- fields dependent on `Dom.AudioFeed`
        channelTitle = afTitle
      , channelLink = urlToURI afLink
      , channelItems
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

mkRssItem :: (Member (Input Port) r) => Dom.AudioFeedItem -> Sem r RssItem'
mkRssItem Dom.AudioFeedItem{afiTitle, afiGuid, afiPubDate, afiDescription, afiLink} = do
  port <- input
  pure
    RssItem
      { itemTitle = afiTitle
      , itemLink = Just . urlToURI $ afiLink
      , itemDescription = afiDescription
      , itemEnclosure = itemEnclosure port
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
    itemEnclosure port =
      [ RssEnclosure
          { enclosureUrl =
              urlToURI $
                "http://localhost:" <> (T.pack . show . portNumber $ port) <> "/yt/" <> getYoutubeVideoId afiGuid
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
