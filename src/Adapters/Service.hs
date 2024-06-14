{-# OPTIONS_GHC -Wno-orphans #-}

module Adapters.Service (API, api, server, AudioServerError (..), err444NoResponse, err429TooManyRequests)
where

import Conduit
import Control.Concurrent.MVar
import Data.Binary.Builder qualified as BB
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Domain.AudioFeed.Item qualified as Dom
import Domain.YoutubeFeed qualified as Dom.YoutubeFeed
import Domain.YoutubeVideoId qualified as Dom
import Network.HTTP.Media ((//))
import Network.HTTP.Types (urlEncode)
import Polysemy
import Polysemy.AtomicState
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import Servant
import Servant.Conduit ()
import URI.ByteString (Host, Port)
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
        , Input (Host, Port)
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
        , Input (Host, Port)
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
        TE.decodeUtf8 . urlEncode True . TE.encodeUtf8 $
          mconcat
            -- date is first because it provides the natural lexicographic order
            [ T.pack . iso8601Show $ utctDay pubDate
            , "_"
            , -- videoId is second because it may be useful for debugging and the
              -- title may be too long for a filename
              Dom.getYoutubeVideoId videoId
            , "_"
            , titleToFilename title
            ]

      titleToFilename = escapeFilename . sanitizeFilename
      -- replaces slashes with underscores because that seems to be the only
      -- forbidden characers in macos/linux filesystems
      sanitizeFilename = T.replace "/" "_"
      {- URL-encodes the filename — this is needed so that gPodder
       - doesn't truncate the filename at `;` or `#` (or others):
       -
       - $ python3
       - >>> import email
       - >>> msg=email.message_from_string("Content-Type:audio/mpeg\nContent-Disposition:attachment; filename*=UTF-8''2024%2D04%2D12%5Frb5oiwx%5FY%2Dc%5F%5F1611%20%D0%97%D0%B0%D1%87%D0%B5%D0%BC%20%D0%BD%D1%83%D0%B6%D0%BD%D1%8B%20%D1%82%D1%83%D1%80%D0%BF%D0%BE%D0%B5%D0%B7%D0%B4%D0%BA%D0%B8%3B%20%D0%9C%D0%BE%D0%B1%D0%B8%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D1%8F%20%D0%B2%20%D0%A3%D0%BA%D1%80%D0%B0%D0%B8%D0%BD%D0%B5.mp3")
       - >>> v = msg.get_param('filename', header='content-disposition')
       - >>> v
       - ('UTF-8', '', '2024-04-12_rb5oiwx_Y-c__1611 Ð\x97Ð°Ñ\x87ÐµÐ¼ Ð½Ñ\x83Ð¶Ð½Ñ\x8b Ñ\x82Ñ\x83Ñ\x80Ð¿Ð¾ÐµÐ·Ð´ÐºÐ¸; Ð\x9cÐ¾Ð±Ð¸Ð»Ð¸Ð·Ð°Ñ\x86Ð¸Ñ\x8f Ð² Ð£ÐºÑ\x80Ð°Ð¸Ð½Ðµ.mp3')
       - >>> f = email.utils.collapse_rfc2231_value(v)
       - >>> f
       - '2024-04-12_rb5oiwx_Y-c__1611 Зачем нужны турпоездки; Мобилизация в Украине.mp3'
       - >>> import urllib
       - >>> urllib.parse.urlparse(f)
       - ParseResult(scheme='', netloc='', path='2024-04-12_rb5oiwx_Y-c__1611 Зачем нужны турпоездки', params=' Мобилизация в Украине.mp3', query='', fragment='')
       -
       - >>> f = email.utils.collapse_rfc2231_value('2024-04-12_rb5oiwx_Y-c__1611 Зачем нужны турпоездки%3B Мобилизация в Украине.mp3')
       - >>> f
       - '2024-04-12_rb5oiwx_Y-c__1611 Зачем нужны турпоездки%3B Мобилизация в Украине.mp3'
       - >>> urllib.parse.urlparse(f)
       - ParseResult(scheme='', netloc='', path='2024-04-12_rb5oiwx_Y-c__1611 Зачем нужны турпоездки%3B Мобилизация в Украине.mp3', params='', query='', fragment='')
       - >>> urllib.parse.unquote(urllib.parse.urlparse(f).path)
       - '2024-04-12_rb5oiwx_Y-c__1611 Зачем нужны турпоездки; Мобилизация в Украине.mp3'
       -
       - see gPodder's:
       - * `get_header_param` https://github.com/gpodder/gpodder/blob/3.11.4/src/gpodder/util.py#L2327
       - * `filename_from_url` https://github.com/gpodder/gpodder/blob/3.11.4/src/gpodder/util.py#L989
       -
       - Note that this can't be tested with `curl -OJ` (8.7.1) as it doesn't URL
       - decode the filename, nor does it support `filename*`, only `filename`.
       -}
      escapeFilename = TE.decodeUtf8 . urlEncode False . TE.encodeUtf8
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
