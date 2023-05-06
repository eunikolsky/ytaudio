module Usecases.RunYoutubePure (runYoutubePure) where

import Data.Text (Text)
import Polysemy
import Usecases.Youtube qualified as UC

{- | Runs the `Usecases.Youtube` effect purely: if the `channelId` matches the
provided value, the `retValue` is returned, otherwise an error is thrown.
-}
runYoutubePure :: (UC.ChannelId, Text) -> InterpreterFor UC.Youtube r
runYoutubePure (channelId, retValue) = interpret $ \case
  UC.GetChannelFeed cid
    | cid == channelId -> pure retValue
    | otherwise -> error "unexpected channel ID"
