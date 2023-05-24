{-# LANGUAGE RecordWildCards #-}

module Domain.YtDlpChannelItems
  ( parse
  ) where

import Data.Aeson ((.:))
import Data.Aeson.Types qualified as A
import Data.Time.Clock
import Data.Time.Format
import Domain.AudioFeed.Item
import Domain.YoutubeVideoId

parse :: A.Value -> A.Parser AudioFeedItem
parse = A.withObject "AudioFeedItem" $ \o -> do
  afiTitle <- o .: "title"
  afiGuid <- YoutubeVideoId <$> o .: "id"
  afiPubDate <- parseUploadDate =<< o .: "upload_date"
  afiDescription <- o .: "description"
  afiLink <- o .: "webpage_url"
  pure AudioFeedItem{..}

{- | Parses `upload_date` field from `yt-dlp` output, which is a string in the
`YYYYMMDD` format, there is no time, so it's set to midnight UTC.
-}
parseUploadDate :: String -> A.Parser UTCTime
parseUploadDate s = case parseTimeM True defaultTimeLocale "%Y%m%d" s of
  Just t -> pure t
  Nothing -> fail $ "Failed to parse upload_date: " <> s
