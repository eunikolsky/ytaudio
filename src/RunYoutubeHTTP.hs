module RunYoutubeHTTP (runYoutubeHTTP) where

-- FIXME move to `External` directory?

import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TEL
import Domain.YtDlpChannelStreams qualified as Dom.YtDlpChannelStreams
import Network.HTTP.Simple
import Polysemy
import System.Process
import Usecases.FeedConfig qualified as UC
import Usecases.Youtube qualified as UC

{- | Interprets the `Usecases.Youtube` effect in terms of external HTTP calls
(youtube RSS and `yt-dlp`).
-}
runYoutubeHTTP :: (Member (Embed IO) r) => Sem (UC.Youtube : r) a -> Sem r a
-- FIXME test this
runYoutubeHTTP = interpret $ \case
  UC.GetChannelFeed (UC.ChannelId cid) -> do
    -- TODO how does it handle errors?
    url <-
      embed . parseRequest . T.unpack $ "https://www.youtube.com/feeds/videos.xml?channel_id=" <> cid
    response <- embed $ httpLBS url
    pure $ TL.toStrict . TEL.decodeUtf8 . getResponseBody $ response
  UC.GetChannelStreams cid -> do
    -- TODO dedup with `RunLiveStreamCheckYtDlp`
    let stdin = ""
    -- TODO check exit code manually
    statusesString <-
      embed $
        readCreateProcess
          (getChannelStreamsProc cid)
          stdin
    let bytestring = TEL.encodeUtf8 . TL.pack $ statusesString
    pure $ Dom.YtDlpChannelStreams.parse bytestring

getChannelStreamsProc :: UC.ChannelId -> CreateProcess
getChannelStreamsProc (UC.ChannelId cid) =
  proc
    "yt-dlp"
    [ "--ignore-no-formats-error"
    , "--no-check-formats"
    , "--no-warnings"
    , "-f"
    , "ba"
    , "-O"
    , "%(.{id,live_status})+j"
    , "-I"
    , ":" <> show maxRequestedStreams
    , "https://www.youtube.com/channel/" <> T.unpack cid
    ]

{- | The number of streams to request from `yt-dlp`. This is a tradeoff between
the number of streams to check and the time it takes to check them. Youtube's
feed contains up to 15 items, but (I think) it's highly unlikely that all of
them will be upcoming streams, so 3 newest streams should be enough.
-}
maxRequestedStreams :: Int
maxRequestedStreams = 3
