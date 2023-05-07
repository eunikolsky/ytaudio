{-# OPTIONS_GHC -Wno-orphans #-}

module Adapters.Service (API, api, server)
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
import Usecases.StreamAudio qualified as UC (streamAudio)
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

server
  :: ( Member UC.Youtube r
     , Member (Error UC.DownloadAudioFeedError) r
     , Member (Input Port) r
     , Member UC.EncodeAudio r
     )
  => ServerT API (Sem r)
server = getAudioFeed :<|> streamAudio

getAudioFeed
  :: (Member UC.Youtube r, Member (Error UC.DownloadAudioFeedError) r, Member (Input Port) r)
  => ChannelId
  -> Sem r RssDocument'
getAudioFeed = UC.downloadAudioFeed Dom.YoutubeFeed.parse . unChannelId

streamAudio
  :: (Member UC.EncodeAudio r)
  => Dom.YoutubeVideoId
  -> Sem r (Headers '[Header "Content-Disposition" Text] (ConduitT () ByteString (ResourceT IO) ()))
streamAudio videoId = addFilenameHeader videoId <$> UC.streamAudio videoId

addFilenameHeader :: (AddHeader h Text orig new) => Dom.YoutubeVideoId -> orig -> new
addFilenameHeader videoId = addHeader ("attachment; filename=\"" <> Dom.getYoutubeVideoId videoId <> ".mp3\"")
