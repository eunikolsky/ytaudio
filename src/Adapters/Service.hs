module Adapters.Service (api, server)
where

import Data.Text (Text)
import Servant
import Usecases.Youtube qualified as UC

{- | Wrapper for `UseCases.ChannelId` in order to implement `FromHttpApiData`,
which is required by `Capture`.
-}
newtype ChannelId = ChannelId {unChannelId :: UC.ChannelId}

instance FromHttpApiData ChannelId where
  parseUrlPiece = fmap (ChannelId . UC.ChannelId) . parseUrlPiece

type API = "feed" :> Capture "channelId" ChannelId :> Get '[PlainText] Text

api :: Proxy API
api = Proxy

server :: Server API
server = getAudioFeed

getAudioFeed :: ChannelId -> Handler Text
getAudioFeed = pure . UC.unChannelId . unChannelId
