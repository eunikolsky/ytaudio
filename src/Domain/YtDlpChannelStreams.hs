module Domain.YtDlpChannelStreams
  ( parse
  , Streams (..)
  ) where

import Conduit
import Data.Aeson ((.:))
import Data.Aeson.Types qualified as A
import Data.ByteString.Lazy qualified as LBS (ByteString)
import Data.Conduit.Binary (sourceLbs)
import Data.Conduit.JSON.NewlineDelimited qualified as ND
import Data.Map (Map)
import Data.Map qualified as M
import Domain.AudioFeed.Item (YoutubeVideoId (YoutubeVideoId))
import Domain.LiveStatus

-- | Map of Youtube video ids to their live status.
newtype Streams = Streams (Map YoutubeVideoId LiveStatus)
  deriving stock (Show, Eq)

{- | Parses `yt-dlp`'s output for the live stream status of channel's streams,
which is newline-separated json objects.
This is needed so that the audio feed can filter out upcoming streams, since
they can't be downloaded (at that point in time) anyway.
-}
parse :: LBS.ByteString -> Streams
parse bs =
  fromList . runConduitPure $
    sourceLbs bs .| mapOutputMaybe (A.parseMaybe parseStreamStatus) ND.valueParser .| sinkList
  where
    -- TODO fold conduit directly into `Streams`
    fromList :: [(YoutubeVideoId, LiveStatus)] -> Streams
    fromList = Streams . M.fromList

{-
Types (simplified) and function composition:

sourceLbs :: Lazy.ByteString -> ConduitT i ByteString m ()
valueParser :: ConduitT ByteString Value m ()
mapOutputMaybe :: (o1 -> Maybe o2) -> ConduitT i o1 m r -> ConduitT i o2 m r
parseMaybe :: (a -> Parser b) -> a -> Maybe b
sinkList :: ConduitT a o m [a]

parseMaybe parseStreamStatus :: Value -> Maybe (YoutubeVideoId, LiveStatus)
mapOutputMaybe (parseMaybe parseStreamStatus) :: ConduitT i Value m r -> ConduitT i (YoutubeVideoId, LiveStatus) m r
mapOutputMaybe (parseMaybe parseStreamStatus) valueParser :: ConduitT ByteString (YoutubeVideoId, LiveStatus) m r
sourceLbs x .| mapOutputMaybe (parseMaybe parseStreamStatus) valueParser :: ConduitT i (YoutubeVideoId, LiveStatus) m r
sourceLbs x .| mapOutputMaybe (parseMaybe parseStreamStatus) valueParser .| sinkList :: ConduitT i o m [(YoutubeVideoId, LiveStatus)]
 -}

parseStreamStatus :: A.Value -> A.Parser (YoutubeVideoId, LiveStatus)
parseStreamStatus = A.withObject "Stream" $ \o -> do
  videoId <- YoutubeVideoId <$> o .: "id"
  status <- o .: "live_status"
  -- TODO make `LiveStatus` directly parseable from JSON
  case liveStatusFromString status of
    Just s -> pure (videoId, s)
    Nothing -> fail $ "Unknown live status: " <> status
