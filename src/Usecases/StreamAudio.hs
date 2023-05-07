module Usecases.StreamAudio (streamAudio) where

import Conduit
import Data.ByteString (ByteString)
import Domain.AudioFeed.Item qualified as Dom
import Polysemy

import Usecases.EncodeAudio

-- FIXME add tests
streamAudio
  :: (Member EncodeAudio r) => Dom.YoutubeVideoId -> Sem r (ConduitT () ByteString (ResourceT IO) ())
-- it seems silly to define this function as only calling to `encodeAudio`,
-- do I need it?
streamAudio = encodeAudio
