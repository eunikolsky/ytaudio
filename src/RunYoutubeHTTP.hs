module RunYoutubeHTTP (runYoutubeHTTP) where

-- FIXME move to `External` directory?

import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TEL
import Network.HTTP.Simple
import Polysemy
import Usecases.Youtube qualified as UC

-- | Interprets the `Usecases.Youtube` effect in terms of `http-client`.
runYoutubeHTTP :: (Member (Embed IO) r) => Sem (UC.Youtube : r) a -> Sem r a
-- FIXME test this
runYoutubeHTTP = interpret $ \case
  UC.GetChannelFeed (UC.ChannelId cid) -> do
    -- TODO how does it handle errors?
    url <-
      embed . parseRequest . T.unpack $ "https://www.youtube.com/feeds/videos.xml?channel_id=" <> cid
    response <- embed $ httpLBS url
    pure $ TL.toStrict . TEL.decodeUtf8 . getResponseBody $ response
