{-# OPTIONS_GHC -Wno-orphans #-}

module Adapters.Service (API, api, server)
where

import Conduit
import Data.Binary.Builder qualified as BB
import Domain.YoutubeFeed qualified as Dom.YoutubeFeed
import Polysemy
import Polysemy.Error
import Servant
import Text.RSS.Conduit.Render (renderRssDocument)
import Text.RSS.Types
import Text.XML.Stream.Render
import Usecases.AudioFeed qualified as UC
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

-- FIXME `application/rss+xml`
type API = "feed" :> Capture "channelId" ChannelId :> Get '[PlainText] RssDocument'

api :: Proxy API
api = Proxy

server :: (Member UC.Youtube r, Member (Error UC.DownloadAudioFeedError) r) => ServerT API (Sem r)
server = getAudioFeed

getAudioFeed
  :: (Member UC.Youtube r, Member (Error UC.DownloadAudioFeedError) r) => ChannelId -> Sem r RssDocument'
getAudioFeed = UC.downloadAudioFeed Dom.YoutubeFeed.parse . unChannelId
