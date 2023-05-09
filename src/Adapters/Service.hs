{-# OPTIONS_GHC -Wno-orphans #-}

module Adapters.Service (API, api, server, AudioServerError (..), err444)
where

import Conduit
import Data.Binary.Builder qualified as BB
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Domain.AudioFeed.Item qualified as Dom
import Domain.YoutubeFeed qualified as Dom.YoutubeFeed
import Network.HTTP.Media ((//))
import Polysemy
import Polysemy.Error
import Polysemy.Input
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

server
  :: ( Member UC.Youtube r
     , Member (Input Port) r
     , Member UC.EncodeAudio r
     , Member (Error AudioServerError) r
     , Member UC.LiveStreamCheck r
     )
  => ServerT API (Sem r)
server = getAudioFeed :<|> streamAudio

getAudioFeed
  :: (Member UC.Youtube r, Member (Error AudioServerError) r, Member (Input Port) r)
  => ChannelId
  -> Sem r RssDocument'
getAudioFeed = mapError DownloadAudioFeedError . UC.downloadAudioFeed Dom.YoutubeFeed.parse . unChannelId

streamAudio
  :: (Member UC.EncodeAudio r, Member (Error AudioServerError) r, Member UC.LiveStreamCheck r)
  => Dom.YoutubeVideoId
  -> Sem r (Headers '[Header "Content-Disposition" Text] (ConduitT () ByteString (ResourceT IO) ()))
streamAudio videoId = addFilenameHeader videoId <$> mapError StreamAudioError (UC.streamAudio videoId)

addFilenameHeader :: (AddHeader h Text orig new) => Dom.YoutubeVideoId -> orig -> new
addFilenameHeader videoId = addHeader ("attachment; filename=\"" <> Dom.getYoutubeVideoId videoId <> ".mp3\"")

err444 :: ServerError
err444 =
  ServerError
    { errHTTPCode = 444
    , errReasonPhrase = "No Response"
    , errBody = ""
    , errHeaders = []
    }
