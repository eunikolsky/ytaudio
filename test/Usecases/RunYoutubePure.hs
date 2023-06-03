module Usecases.RunYoutubePure (runYoutubePure) where

import Data.Text (Text)
import Domain.YtDlpChannelStreams qualified as Dom
import Polysemy
import Usecases.FeedConfig qualified as UC
import Usecases.Youtube qualified as UC

{- | Runs the `Usecases.Youtube` effect purely: if the `channelId` matches the
provided value, the `retValue` is returned, otherwise an error is thrown.
-}
runYoutubePure :: (UC.ChannelId, Text, Dom.Streams) -> InterpreterFor UC.Youtube r
runYoutubePure (channelId, retValue, streams) = interpret $ \case
  UC.GetChannelFeed cid
    | cid == channelId -> pure retValue
    | otherwise -> error "GetChannelFeed: unexpected channel ID"
  UC.GetChannelStreams cid
    | cid == channelId -> pure streams
    | otherwise -> error "GetChannelStreams: unexpected channel ID"
  UC.StreamChannelItems cid
    | cid == channelId -> pure $ pure ()
    | otherwise -> error "StreamChannelItems: unexpected channel ID"
