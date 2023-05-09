{-# LANGUAGE TemplateHaskell #-}

module Usecases.LiveStreamCheck
  ( LiveStreamCheck (..)
  , liveStreamCheck
  ) where

import Domain.AudioFeed.Item qualified as Dom
import Domain.LiveStatus qualified as Dom
import Polysemy

data LiveStreamCheck m a where
  LiveStreamCheck :: Dom.YoutubeVideoId -> LiveStreamCheck m Dom.LiveStatus

makeSem ''LiveStreamCheck
