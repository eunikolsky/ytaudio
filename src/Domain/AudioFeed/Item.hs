{-# LANGUAGE RecordWildCards #-}

module Domain.AudioFeed.Item (AudioFeedItem (..))
where

import Data.Text (Text)
import Data.Time.Clock
import Domain.YoutubeVideoId
import Text.Show.Unicode

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
  deriving stock (Eq)

-- I have to define `Show AudioFeedItem` manually in order to show Unicode
-- characters PROPERLY!
instance Show AudioFeedItem where
  show AudioFeedItem{..} =
    mconcat
      [ "AudioFeedItem(title="
      , ushow afiTitle
      , ", guid="
      , show afiGuid
      , ", pubDate="
      , show afiPubDate
      , ", description="
      , ushow afiDescription
      , ", link="
      , show afiLink
      , ")"
      ]
