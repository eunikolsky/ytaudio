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
  deriving newtype (Show, Eq)

-- `deriving newtype (Generic)` doesn't work: `No instance for (Generic Text)` ¯\_(ツ)_/¯

instance ToJSON ChannelId where
  toJSON = toJSON . unChannelId
  toEncoding = toEncoding . unChannelId

data FeedConfig = FeedConfig
  { channelId :: !ChannelId
  , allEpisodes :: !Bool
  -- ^ whether to download all episodes (using `yt-dlp`)
  }
  deriving stock (Generic)

instance ToJSON FeedConfig where
  toEncoding = genericToEncoding defaultOptions
