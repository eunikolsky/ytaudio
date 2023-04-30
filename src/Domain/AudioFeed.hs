module Domain.AudioFeed (AudioFeed (..))
where

import Data.Text (Text)
import Domain.AudioFeed.Item

{- | Model of an audio feed with the data that can be put into RSS and can be
parsed from a Youtube feed.

See the RSS 2.0 specification at <https://www.rssboard.org/rss-specification>.
-}
data AudioFeed = AudioFeed
  { afTitle :: !Text
  , afDescription :: !Text
  , afLink :: !Text
  -- ^ URL of the original youtube channel
  , afItems :: ![AudioFeedItem]
  }
  deriving stock (Show)
