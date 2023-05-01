{-# LANGUAGE RecordWildCards #-}

module Domain.AudioFeed (AudioFeed (..))
where

import Data.Text (Text)
import Domain.AudioFeed.Item
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
