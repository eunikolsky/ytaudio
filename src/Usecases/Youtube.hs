{-# LANGUAGE TemplateHaskell #-}

module Usecases.Youtube (Youtube (..), ChannelId (..), getChannelFeed)
where

import Data.Text (Text)
import Polysemy

-- | Youtube channel ID.
newtype ChannelId = ChannelId {unChannelId :: Text}
  deriving newtype (Show, Eq)

-- | Effect that provides access to youtube.
data Youtube m a where
  -- TODO should the return type be wrapped in Either to handle errors, or
  -- should that be done by an interpreter with the `Error` effect?

  -- | Downloads a youtube channel feed.
  GetChannelFeed :: ChannelId -> Youtube m Text

makeSem ''Youtube
