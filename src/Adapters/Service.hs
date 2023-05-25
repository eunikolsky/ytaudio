{-# OPTIONS_GHC -Wno-orphans #-}

module Adapters.Service (API, api, server, AudioServerError (..), err444NoResponse, err429TooManyRequests)
where

import Conduit
import Control.Concurrent.MVar
import Data.Binary.Builder qualified as BB
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Domain.YoutubeFeed qualified as Dom.YoutubeFeed
import Domain.YoutubeVideoId qualified as Dom
import Network.HTTP.Media ((//))
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import Servant
import Servant.Conduit ()
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
        , Embed IO
        ]
        r
     )
  => MVar ()
  -> ServerT API (Sem r)
server concurrentLock = getAudioFeed :<|> streamAudio concurrentLock

getAudioFeed
  :: (Members [UC.Youtube, Error AudioServerError, Input Port] r, Monad m)
  => ChannelId
  -> Sem r (ConduitT () BB.Builder m ())
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
  :: (Members [UC.EncodeAudio, Error AudioServerError, UC.LiveStreamCheck, Resource, Embed IO] r)
  => MVar ()
  -> Dom.YoutubeVideoId
  -> Sem r (Headers '[Header "Content-Disposition" Text] (ConduitT () ByteString (ResourceT IO) ()))
-- TODO don't add the filename header if there is an error

-- the response code has two parts: preparing the conduit (which is done by
-- `UC.streamAudio` in this function) and running the conduit (which is done
-- after this function returns). This means that a `bracket` here works only
-- within the first part.
streamAudio concurrentLock videoId =
  let response = addFilenameHeader videoId <$> conduit
      conduit = mapError StreamAudioError (releaseLockAfterConduit concurrentLock <$> UC.streamAudio videoId)
  in tryTakeLock
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
    releaseIfError Nothing = do pure ()

    -- run `lockedAction` if the lock was taken; otherwise, run `notLockedAction`
    action (Just _) = lockedAction
    action Nothing = notLockedAction

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
