{-# OPTIONS_GHC -Wno-orphans #-}

module Adapters.Service (API, api, server, AudioServerError (..), err444NoResponse, err429TooManyRequests)
where

import Conduit
import Control.Concurrent.MVar
import Data.Binary.Builder (toLazyByteString)
import Data.Binary.Builder qualified as BB
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Domain.AudioFeed.Item qualified as Dom
import Domain.YoutubeFeed qualified as Dom.YoutubeFeed
import Domain.YoutubeVideoId qualified as Dom
import Network.HTTP.Media ((//))
import Polysemy
import Polysemy.AtomicState
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import Servant
import Servant.Conduit ()
import URI.ByteString (Port, urlEncode)
import Usecases.AudioFeed qualified as UC
import Usecases.EncodeAudio qualified as UC
import Usecases.FeedConfig qualified as UC
import Usecases.GetFeedConfig qualified as UC
import Usecases.LiveStreamCheck qualified as UC
import Usecases.StreamAudio qualified as UC
import Usecases.Youtube qualified as UC

{- | Wrapper for `UseCases.ChannelId` in order to implement `FromHttpApiData`,
which is required by `Capture`.
-}
newtype ChannelId = ChannelId {unChannelId :: UC.ChannelId}

instance FromHttpApiData ChannelId where
  parseUrlPiece = fmap (ChannelId . UC.ChannelId) . parseUrlPiece

instance MimeRender PlainText BB.Builder where
  mimeRender _ = BB.toLazyByteString

instance FromHttpApiData Dom.YoutubeVideoId where
  parseUrlPiece = fmap Dom.YoutubeVideoId . parseUrlPiece

data MP3

instance Accept MP3 where
  contentType _ = "audio" // "mpeg"

instance MimeRender MP3 ByteString where
  mimeRender _ = BS.fromStrict

-- FIXME `application/rss+xml`
type API =
  "feed"
    :> Capture "channelId" ChannelId
    :> StreamGet NoFraming PlainText (ConduitT () BB.Builder IO ())
    :<|> "feed"
      :> Capture "channelId" ChannelId
      :> "config"
      :> Get '[JSON] UC.FeedConfig
    :<|> "feed"
      :> Capture "channelId" ChannelId
      :> "config"
      :> ReqBody '[JSON] UC.FeedConfig
      :> Post '[JSON] UC.FeedConfig
    :<|> "yt"
      :> Capture "videoid" Dom.YoutubeVideoId
      :> StreamGet
          NoFraming
          MP3
          (Headers '[Header "Content-Disposition" Text] (ConduitT () ByteString (ResourceT IO) ()))

api :: Proxy API
api = Proxy

{- | Combination of errors from all server's usecases.
TODO it's probably not the responsibility of the server to combine errors?!
-}
data AudioServerError
  = DownloadAudioFeedError UC.DownloadAudioFeedError
  | StreamAudioError UC.StreamAudioError
  | ConcurrentStreamingError

server
  :: ( Members
        [ UC.Youtube
        , Input Port
        , UC.EncodeAudio
        , Error AudioServerError
        , UC.LiveStreamCheck
        , AtomicState UC.FullChannels
        , AtomicState UC.AudioFeedItems
        , Resource
        , Embed IO
        ]
        r
     )
  => MVar ()
  -> ServerT API (Sem r)
server concurrentLock = getAudioFeed :<|> getFeedConfig :<|> postFeedConfig :<|> streamAudio concurrentLock

getAudioFeed
  :: ( Members
        [ UC.Youtube
        , Error AudioServerError
        , Input Port
        , AtomicState UC.FullChannels
        , AtomicState UC.AudioFeedItems
        ]
        r
     )
  => ChannelId
  -> Sem r (ConduitT () BB.Builder IO ())
getAudioFeed = mapError DownloadAudioFeedError . UC.downloadAudioFeed Dom.YoutubeFeed.parse . unChannelId

getFeedConfig :: (Member (AtomicState UC.FullChannels) r) => ChannelId -> Sem r UC.FeedConfig
getFeedConfig = UC.getFeedConfig . unChannelId

postFeedConfig
  :: (Member (AtomicState UC.FullChannels) r) => ChannelId -> UC.FeedConfig -> Sem r UC.FeedConfig
postFeedConfig cid = UC.changeFeedConfig (unChannelId cid)

{- | Re-encode and stream audio to the client. Since the usecase is not
concurrent-friendly [1] at the moment, the server has to allow only one mp3
download at a time, other requests will get 429 Too many requests.
[1] `yt-dlp` creates a temporary file for each fragment of the audio stream
(a fragment is 10 MiB by default), named `--FragN` in our case (the first `-`
means `stdout`). So concurrent downloads would have conflicts for the same
files. The `--no-part` option doesn't work for fragments
(https://github.com/yt-dlp/yt-dlp/issues/2783#issuecomment-1464951913). There
is an interesting workaround by using `--downloader dash:ffmpeg`
(https://github.com/yt-dlp/yt-dlp/issues/2783#issuecomment-1464866629), which
doesn't create temporary files, but the download speed is much slower than
with the "native" downloader: the fact that the latter splits the downloads
into parts allows good download speeds; the ffmpeg's reencoding speed can be
66× (native) compared to 2× (ffmpeg). I've only tested this with audio-only
dash formats since they are present even in non-live videos. This issue may
or may not be dash-specific.
-}
streamAudio
  -- TODO should this require a more specific "Locking" effect rather than "Resource"?
  :: ( Members
        [ UC.EncodeAudio
        , Error AudioServerError
        , UC.LiveStreamCheck
        , AtomicState UC.AudioFeedItems
        , Resource
        , Embed IO
        ]
        r
     )
  => MVar ()
  -> Dom.YoutubeVideoId
  -> Sem r (Headers '[Header "Content-Disposition" Text] (ConduitT () ByteString (ResourceT IO) ()))
-- TODO don't add the filename header if there is an error

-- the response code has two parts: preparing the conduit (which is done by
-- `UC.streamAudio` in this function) and running the conduit (which is done
-- after this function returns). This means that a `bracket` here works only
-- within the first part.
streamAudio concurrentLock videoId = do
  feedItems <- atomicGet
  let maybeFeedItem = feedItems M.!? videoId
      feedItem = maybe (FilenameVideoId videoId) FilenameFeedItem maybeFeedItem
      response = addFilenameHeader feedItem <$> conduit
      conduit = mapError StreamAudioError (releaseLockAfterConduit concurrentLock <$> UC.streamAudio videoId)
  tryTakeLock
    concurrentLock
    response
    (throw ConcurrentStreamingError)

{- | Makes sure the lock is released after the conduit finishes.

Warning: the lock must be taken before calling this function.
-}
releaseLockAfterConduit
  -- FIXME is it possible to use the `Sem r` monad in the conduit so that its
  -- effect could be used, e.g. tracing?
  :: MVar () -> ConduitT () ByteString (ResourceT IO) () -> ConduitT () ByteString (ResourceT IO) ()
releaseLockAfterConduit lock conduit = bracketP acquire release (const conduit)
  where
    -- if we're here, the lock was already taken, so nothing to do here
    acquire = pure ()
    -- always release the lock when the response finishes
    release _ = putMVar lock ()

{- | Tries to take the (concurrent) lock, runs `lockedAction` if the lock was
taken, otherwise runs `notLockedAction`.

Warning: the lock is released only if an error occurs in `lockedAction`, so
the caller must release the lock if there is no error.
-}
tryTakeLock
  :: (Members [Resource, Embed IO] r)
  => MVar ()
  -> Sem r a
  -- ^ action to run if the lock was taken
  -> Sem r a
  -- ^ action to run if the lock was not taken
  -> Sem r a
tryTakeLock lock lockedAction notLockedAction = bracketOnError acquire releaseIfError action
  where
    -- try taking the lock, this is non-blocking
    acquire = embed $ tryTakeMVar lock

    -- !only if error! put the lock back if it was taken
    releaseIfError (Just _) = embed $ putMVar lock ()
    releaseIfError Nothing = pure ()

    -- run `lockedAction` if the lock was taken; otherwise, run `notLockedAction`
    action (Just _) = lockedAction
    action Nothing = notLockedAction

data FilenameSource = FilenameFeedItem Dom.AudioFeedItem | FilenameVideoId Dom.YoutubeVideoId

addFilenameHeader :: (AddHeader h Text orig new) => FilenameSource -> orig -> new
addFilenameHeader
  ( FilenameFeedItem
      (Dom.AudioFeedItem{Dom.afiGuid = videoId, Dom.afiTitle = title, Dom.afiPubDate = pubDate})
    ) =
    -- note: this uses an "extended notation" from RFC 8187 [0] in order to
    -- encode non-US-ASCII characters; gPodder does create the file with the
    -- broken encoding in the name, but renames it to the correct one when the
    -- download finishes
    -- [0]: https://www.rfc-editor.org/rfc/rfc8187.html#section-3.2.3
    addHeader $ "attachment; filename*=UTF-8''" <> encodedFilename <> ".mp3"
    where
      encodedFilename =
        TE.decodeUtf8 . BS.toStrict . toLazyByteString . urlEncode mempty . TE.encodeUtf8 $
          mconcat
            -- date is first because it provides the natural lexicographic order
            [ T.pack . iso8601Show $ utctDay pubDate
            , "_"
            , -- videoId is second because it may be useful for debugging and the
              -- title may be too long for a filename
              Dom.getYoutubeVideoId videoId
            , "_"
            , sanitizedTitle
            ]
      -- this replaces (consequent) unsafe URL characters with an underscore,
      -- which is needed to have full filenames because gPodder extracts the
      -- filename from the already decoded Content-Disposition filename by
      -- parsing it as a URL and using the URL path, so a `#` causes
      -- incomplete filenames since it starts a URL fragment
      sanitizedTitle = T.intercalate "_" $ T.split (`S.member` unsafeURLChars) title
      -- https://stackoverflow.com/questions/695438/what-are-the-safe-characters-for-making-urls
      unsafeURLChars = S.fromList "+&=?/#%<>[]{}|\\^"
addFilenameHeader (FilenameVideoId videoId) =
  addHeader $
    mconcat
      [ "attachment; filename=\""
      , Dom.getYoutubeVideoId videoId
      , ".mp3\""
      ]

err444NoResponse :: ServerError
err444NoResponse =
  ServerError
    { errHTTPCode = 444
    , errReasonPhrase = "No Response"
    , errBody = ""
    , errHeaders = []
    }

err429TooManyRequests :: ServerError
err429TooManyRequests =
  ServerError
    { errHTTPCode = 429
    , errReasonPhrase = "Too Many Requests"
    , errBody = ""
    , errHeaders = []
    }
