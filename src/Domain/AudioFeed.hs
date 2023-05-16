{-# LANGUAGE RecordWildCards #-}

module Domain.AudioFeed (AudioFeed (..), dropUnavailable)
where

import Data.Map ((!?))
import Data.Text (Text)
import Domain.AudioFeed.Item
import Domain.LiveStatus
import Domain.YtDlpChannelStreams
import Text.Show.Unicode

{- | Model of an audio feed with the data that can be put into RSS and can be
parsed from a Youtube feed.

TODO turns out channel's description is not available in the youtube feed; we
can probably get it from `yt-dlp`, but then it needs to be cached.

See the RSS 2.0 specification at <https://www.rssboard.org/rss-specification>.
-}
data AudioFeed = AudioFeed
  { afTitle :: !Text
  , afLink :: !Text
  -- ^ URL of the original youtube channel
  , afItems :: ![AudioFeedItem]
  }
  deriving stock (Eq)

-- I have to define `Show AudioFeedItem` manually in order to show Unicode
-- characters PROPERLY!
instance Show AudioFeed where
  show AudioFeed{..} =
    mconcat
      [ "AudioFeed(title="
      , ushow afTitle
      , ", link="
      , show afLink
      , ", items="
      , show afItems
      , ")"
      ]

{- | Removes audio feeds items that are upcoming streams and thus can't be
downloaded yet.
-}
dropUnavailable :: Streams -> AudioFeed -> AudioFeed
dropUnavailable (Streams streams) feed =
  feed
    { afItems = filter isAvailable (afItems feed)
    }
  where
    isAvailable :: AudioFeedItem -> Bool
    isAvailable AudioFeedItem{afiGuid} =
      maybe True canBeDownloaded $ streams !? afiGuid
