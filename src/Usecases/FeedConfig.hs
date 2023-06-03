module Usecases.FeedConfig
  ( ChannelId (..)
  , FeedConfig (..)
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- | Youtube channel ID.
newtype ChannelId = ChannelId {unChannelId :: Text}
  deriving newtype (Show, Eq, Ord)

-- `deriving newtype (Generic)` doesn't work: `No instance for (Generic Text)` ¯\_(ツ)_/¯

instance ToJSON ChannelId where
  toJSON = toJSON . unChannelId
  toEncoding = toEncoding . unChannelId

instance FromJSON ChannelId where
  parseJSON = fmap ChannelId . parseJSON

newtype FeedConfig = FeedConfig
  { allEpisodes :: Bool
  -- ^ whether to include all episodes in the feed (using `yt-dlp`, this is much slower!)
  }
  deriving stock (Generic)

instance ToJSON FeedConfig where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FeedConfig where
  parseJSON = genericParseJSON defaultOptions
