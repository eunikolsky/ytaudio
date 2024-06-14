module Usecases.AudioFeed
  ( downloadAudioFeed
  , AudioFeedItems
  , DownloadAudioFeedError (..)
  )
where

-- `Domain` types are imported with the `Dom.` prefix, whereas functions and
-- field names are imported directly to reduce noise

import Conduit
import Data.Binary.Builder qualified as BB
import Data.Either
import Data.Map (Map)
import Data.Map qualified as M
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
import Domain.YoutubeVideoId qualified as Dom
import Polysemy
import Polysemy.AtomicState
import Polysemy.Error
import Polysemy.Input
import Text.RSS.Conduit.Render
import Text.RSS.Types
import Text.XML.Stream.Render
import URI.ByteString
import Usecases.FeedConfig
import Usecases.GetFeedConfig
import Usecases.RssConduitExt
import Usecases.Youtube

-- | Errors that can happen in the `downloadAudioFeed` usecase.
newtype DownloadAudioFeedError = YoutubeFeedParseError Text
  deriving stock (Show, Eq)

{- | List of feed items known so far. It's appended to when audio feeds are
requested; feed items from all feeds go into the same list because the
youtube video id is unique in the system. An item from this list is used to
get video's information by id (date and title) for the audio filename.
-}
type AudioFeedItems = Map Dom.YoutubeVideoId Dom.AudioFeedItem

-- Audio feed on the `Usecases` layer is a conduit of byte strings (builders)
-- because all the feed's items may not be available immediately and we need to
-- stream them as they become available.
downloadAudioFeed
  -- TODO `Port` comes from `uri-bytestring` — is this fine?
  :: ( Members
        [ Youtube
        , Error DownloadAudioFeedError
        , Input (Host, Port)
        , AtomicState FullChannels
        , AtomicState AudioFeedItems
        ]
        r
     )
  => (Text -> Maybe Dom.AudioFeed)
  -> ChannelId
  -> Sem r (ConduitT () BB.Builder IO ())
-- TODO should `parseFeed` be a separate effect, even though it's a pure function?
downloadAudioFeed parseFeed channelId = do
  fullFeed <- atomicGets (isFullChannel channelId)
  if fullFeed then downloadFullAudioFeed else downloadLatestAudioFeed
  where
    downloadLatestAudioFeed = do
      feed <- getChannelFeed channelId
      case parseFeed feed of
        Just originalAudioFeed -> do
          maybeLocalItems <- getLocalChannelItems channelId
          let audioFeed = maybe originalAudioFeed (appendItems originalAudioFeed) maybeLocalItems
          -- TODO download these in parallel
          streams <- getChannelStreams channelId
          let downloadableAudioFeed = Dom.dropUnavailable streams audioFeed
          atomicModify' @AudioFeedItems (appendFeedItems $ afItems downloadableAudioFeed)

          doc <- mkRssDoc downloadableAudioFeed
          pure $ renderRssDocument doc .| renderBuilder def
        Nothing -> throw $ YoutubeFeedParseError feed

    -- M.union prefers the first argument when duplicate keys are encountered
    appendFeedItems newFeedItems = M.union newFeedMap
      where
        newFeedMap = M.fromList $ (\item -> (Dom.afiGuid item, item)) <$> newFeedItems

    downloadFullAudioFeed = do
      itemsC <- streamChannelItems channelId
      -- FIXME this information should really be parsed from the actual feed
      emptyFeed <-
        mkRssDoc $ Dom.AudioFeed{afTitle = "", afLink = "http://not.localhost/", afItems = mempty}
      (host, port) <- input
      pure $
        itemsC .| mapC (mkRssItem (host, port)) .| renderRssDocumentStreaming emptyFeed .| renderBuilder def

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

mkRssDoc :: (Member (Input (Host, Port)) r) => Dom.AudioFeed -> Sem r RssDocument'
mkRssDoc Dom.AudioFeed{afTitle, afLink, afItems} = do
  port <- input
  let channelItems = mkRssItem port <$> afItems
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

-- this function can't use `Sem r` (for `Input Port`) because it's also used in
-- a returned conduit, which is in IO
mkRssItem :: (Host, Port) -> Dom.AudioFeedItem -> RssItem'
mkRssItem (host, port) Dom.AudioFeedItem{afiTitle, afiGuid, afiPubDate, afiDescription, afiLink} =
  RssItem
    { itemTitle = afiTitle <> " [" <> videoId <> "]"
    , itemLink = Just . urlToURI $ afiLink
    , itemDescription = afiDescription
    , itemEnclosure
    , itemGuid = Just . GuidText $ videoId
    , itemPubDate = Just afiPubDate
    , -- unused fields
      itemAuthor = ""
    , itemCategories = []
    , itemComments = Nothing
    , itemSource = Nothing
    , itemExtensions = NoItemExtensions
    }
  where
    videoId = getYoutubeVideoId afiGuid
    itemEnclosure =
      [ RssEnclosure
          { enclosureUrl =
              urlToURI $
                "http://"
                  <> TE.decodeUtf8 (hostBS host)
                  <> ":"
                  <> (T.pack . show . portNumber $ port)
                  <> "/yt/"
                  <> videoId
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
