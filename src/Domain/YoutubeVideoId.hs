module Domain.YoutubeVideoId (YoutubeVideoId (..)) where

import Data.Text (Text)

{- | A unique Youtube video ID that looks like "Y4lOd3L-Uks". It's used as
RSS item's `guid` and `enclosure`'s URL is derived from it.
-}
newtype YoutubeVideoId = YoutubeVideoId {getYoutubeVideoId :: Text}
  deriving newtype (Show, Eq, Ord)
