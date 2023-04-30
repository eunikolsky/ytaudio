module Domain.AudioFeed.Item (AudioFeedItem (..), YoutubeVideoId (..))
where

import Data.Text (Text)
import Data.Time.Clock

{- | A unique Youtube video ID that looks like "Y4lOd3L-Uks". It's used as
RSS item's `guid` and `enclosure`'s URL is derived from it.
-}
newtype YoutubeVideoId = YoutubeVideoId {getYoutubeVideoId :: Text}
  deriving newtype (Show)

{- | Model of an audio feed item with the data that can be put into RSS and can
be parsed from a Youtube feed. Contains only the fields that are changed in
every item, e.g. there is no `enclosure` since it's derived from `guid` at
the Usecases layer.
-}
data AudioFeedItem = AudioFeedItem
  { afiTitle :: !Text
  , afiGuid :: !YoutubeVideoId
  , afiPubDate :: !UTCTime
  , afiDescription :: !Text
  , afiLink :: !Text
  -- ^ URL of the original youtube video
  }
  deriving stock (Show)
