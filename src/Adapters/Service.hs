{-# OPTIONS_GHC -Wno-orphans #-}

module Adapters.Service (API, api, server, AudioServerError (..), err444NoResponse, err429TooManyRequests)
where

import Conduit
import Control.Concurrent.MVar
import Data.Binary.Builder qualified as BB
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Domain.AudioFeed.Item qualified as Dom
import Domain.YoutubeFeed qualified as Dom.YoutubeFeed
import Network.HTTP.Media ((//))
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import Polysemy.Trace
import Servant
import Servant.Conduit ()
import Text.RSS.Conduit.Render (renderRssDocument)
import Text.RSS.Types
import Text.XML.Stream.Render
import URI.ByteString (Port)
import Usecases.AudioFeed qualified as UC
import Usecases.EncodeAudio qualified as UC
import Usecases.LiveStreamCheck qualified as UC
import Usecases.StreamAudio qualified as UC
import Usecases.Youtube qualified as UC

{- | Wrapper for `UseCases.ChannelId` in order to implement `FromHttpApiData`,
which is required by `Capture`.
-}
newtype ChannelId = ChannelId {unChannelId :: UC.ChannelId}

instance FromHttpApiData ChannelId where
  parseUrlPiece = fmap (ChannelId . UC.ChannelId) . parseUrlPiece

-- FIXME move to a separate module
instance MimeRender PlainText RssDocument' where
  -- TODO stream the rendered document directly?
  mimeRender _ doc = BB.toLazyByteString . runConduitPure $ renderRssDocument doc .| renderBuilder def .| foldC

instance FromHttpApiData Dom.YoutubeVideoId where
  parseUrlPiece = fmap Dom.YoutubeVideoId . parseUrlPiece

data MP3

instance Accept MP3 where
  contentType _ = "audio" // "mpeg"

instance MimeRender MP3 ByteString where
  mimeRender _ = BS.fromStrict

-- FIXME `application/rss+xml`
type API =
  "feed" :> Capture "channelId" ChannelId :> Get '[PlainText] RssDocument'
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
        , Resource
        , Trace
        , Embed IO
        ]
        r
     )
  => MVar ()
  -> ServerT API (Sem r)
server concurrentLock = getAudioFeed :<|> streamAudio concurrentLock

getAudioFeed
  :: (Member UC.Youtube r, Member (Error AudioServerError) r, Member (Input Port) r)
  => ChannelId
  -> Sem r RssDocument'
getAudioFeed = mapError DownloadAudioFeedError . UC.downloadAudioFeed Dom.YoutubeFeed.parse . unChannelId

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
  :: (Members [UC.EncodeAudio, Error AudioServerError, UC.LiveStreamCheck, Resource, Trace, Embed IO] r)
  => MVar ()
  -> Dom.YoutubeVideoId
  -> Sem r (Headers '[Header "Content-Disposition" Text] (ConduitT () ByteString (ResourceT IO) ()))
-- TODO don't add the filename header if there is an error
streamAudio concurrentLock videoId = do
  concLocked
    concurrentLock
    videoId
    (addFilenameHeader videoId <$> mapError StreamAudioError (UC.streamAudio videoId))
    (throw ConcurrentStreamingError)

concLocked
  :: (Members [Trace, Resource, Embed IO] r)
  => MVar ()
  -> Dom.YoutubeVideoId
  -> Sem r a
  -> Sem r a
  -> Sem r a
concLocked lock videoId lockedAction notLockedAction = bracket acquire release action
  where
    vid = T.unpack $ Dom.getYoutubeVideoId videoId

    -- try taking the lock, this is non-blocking
    acquire = do
      maybeLock <- embed $ tryTakeMVar lock
      case maybeLock of
        Just _ -> trace $ "Lock taken for " <> vid
        Nothing -> trace $ "Lock not taken for " <> vid
      pure maybeLock

    -- always put the lock back if it was taken
    release (Just _) = do
      trace $ "Lock released for " <> vid
      embed $ putMVar lock ()
    release Nothing = do
      trace $ "No lock to release for " <> vid
      pure ()

    -- run the action if the lock was taken; otherwise, run the exception
    action (Just _) = do
      trace $ "Running locked action for " <> vid
      lockedAction
    action Nothing = do
      trace $ "Running not locked action for " <> vid
      notLockedAction

addFilenameHeader :: (AddHeader h Text orig new) => Dom.YoutubeVideoId -> orig -> new
addFilenameHeader videoId = addHeader ("attachment; filename=\"" <> Dom.getYoutubeVideoId videoId <> ".mp3\"")

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
