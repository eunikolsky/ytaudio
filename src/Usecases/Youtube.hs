{-# LANGUAGE TemplateHaskell #-}

module Usecases.Youtube (Youtube (..), getChannelFeed, getChannelStreams, getLocalChannelItems, streamChannelItems)
where

import Conduit
import Data.Text (Text)
import Domain.AudioFeed.Item qualified as Dom
import Domain.YtDlpChannelStreams qualified as Dom
import Polysemy
import Usecases.FeedConfig

-- | Effect that provides access to youtube.
data Youtube m a where
  -- TODO should the return type be wrapped in Either to handle errors, or
  -- should that be done by an interpreter with the `Error` effect?

  -- | Downloads a youtube channel feed.
  GetChannelFeed :: ChannelId -> Youtube m Text
  -- | Returns the streams status of a youtube channel.
  GetChannelStreams :: ChannelId -> Youtube m Dom.Streams
  -- | Returns the audio feed items from a local file if present; the file is
  -- expected in the current directory with the name "channelId.jsonl". This
  -- file may contain manually-prepared items (with `yt-dlp`) added to the
  -- feed; for example, those that are gone from the main feed, whereas
  -- creating a feed from the entire channel is unnecessary and way too slow.
  GetLocalChannelItems :: ChannelId -> Youtube m (Maybe [Dom.AudioFeedItem])
  -- | Streams feed items from `yt-dlp`.
  StreamChannelItems :: ChannelId -> Youtube m (ConduitT i Dom.AudioFeedItem IO ())

makeSem ''Youtube
