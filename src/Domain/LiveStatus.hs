module Domain.LiveStatus
  ( LiveStatus (..)
  , liveStatusFromString
  , canBeDownloaded
  ) where

{- | Defines the live status of a youtube video as returned by `yt-dlp`.
https://github.com/yt-dlp/yt-dlp#output-template
-}
data LiveStatus
  = NotLive
  | IsLive
  | IsUpcoming
  | WasLive
  | -- | was live, but VOD is not yet processed
    PostLive
  deriving stock (Show, Eq)

-- | Construct a `LiveStatus` from a string.
liveStatusFromString :: String -> Maybe LiveStatus
liveStatusFromString "not_live" = Just NotLive
liveStatusFromString "is_live" = Just IsLive
liveStatusFromString "is_upcoming" = Just IsUpcoming
liveStatusFromString "was_live" = Just WasLive
liveStatusFromString "post_live" = Just PostLive
liveStatusFromString _ = Nothing

-- | Returns whether a video can be downloaded based on its live status.
canBeDownloaded :: LiveStatus -> Bool
canBeDownloaded IsUpcoming = False
-- this case is not clear, I haven't checked whether `yt-dlp` can download it
canBeDownloaded PostLive = True
canBeDownloaded _ = True
