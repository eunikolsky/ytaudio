module Usecases.StreamAudio (streamAudio, StreamAudioError (..)) where

import Conduit
import Data.ByteString (ByteString)
import Domain.LiveStatus qualified as Dom
import Domain.YoutubeVideoId qualified as Dom
import Polysemy
import Polysemy.Error
import Usecases.EncodeAudio
import Usecases.LiveStreamCheck

-- | Errors that can happen in `streamAudio`.
newtype StreamAudioError = LiveStreamNotReady Dom.LiveStatus

{- | Usecase to stream and re-encode an audio stream of a youtube video as MP3.
Live streams are for now checked here before downloading, in which case an
HTTP error is returned instead of producing a couple of warnings to `stderr`
and an empty response body.
-}
streamAudio
  :: (Member EncodeAudio r, Member LiveStreamCheck r, Member (Error StreamAudioError) r)
  => Dom.YoutubeVideoId
  -> Sem r (ConduitT () ByteString (ResourceT IO) ())
-- FIXME add tests
streamAudio videoId = do
  liveStatus <- liveStreamCheck videoId
  if Dom.canBeDownloaded liveStatus
    then encodeAudio videoId
    else throw $ LiveStreamNotReady liveStatus
