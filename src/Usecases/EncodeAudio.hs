{-# LANGUAGE TemplateHaskell #-}

module Usecases.EncodeAudio
  ( EncodeAudio (..)
  , encodeAudio
  ) where

import Conduit
import Data.ByteString (ByteString)
import Domain.YoutubeVideoId qualified as Dom
import Polysemy

-- | Effect that reencodes streaming audio from youtube as MP3.
data EncodeAudio m a where
  -- TODO is `ResourceT IO` fine here??
  EncodeAudio :: Dom.YoutubeVideoId -> EncodeAudio m (ConduitT () ByteString (ResourceT IO) ())

makeSem ''EncodeAudio
