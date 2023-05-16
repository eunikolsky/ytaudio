{-# LANGUAGE TemplateHaskell #-}

module Usecases.LiveStreamCheck
  ( LiveStreamCheck (..)
  , liveStreamCheck
  ) where

import Domain.LiveStatus qualified as Dom
import Domain.YoutubeVideoId qualified as Dom
import Polysemy

data LiveStreamCheck m a where
  LiveStreamCheck :: Dom.YoutubeVideoId -> LiveStreamCheck m Dom.LiveStatus

makeSem ''LiveStreamCheck
