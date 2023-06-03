module Usecases.GetFeedConfig (FullChannels, getFeedConfig, changeFeedConfig) where

import Data.Set (Set)
import Data.Set qualified as S
import Polysemy
import Polysemy.AtomicState
import Usecases.FeedConfig

{- | Set of `ChannelId`s that have been configured to download all episodes
(since the feed config controls only this single property).
-}
newtype FullChannels = FullChannels (Set ChannelId)
  deriving newtype (Semigroup, Monoid)

getFeedConfig :: (Member (AtomicState FullChannels) r) => ChannelId -> Sem r FeedConfig
getFeedConfig cid = do
  allEpisodes <- atomicGets $ \(FullChannels fullChannels) -> S.member cid fullChannels
  pure FeedConfig{allEpisodes}

changeFeedConfig
  :: (Member (AtomicState FullChannels) r) => ChannelId -> FeedConfig -> Sem r FeedConfig
changeFeedConfig cid FeedConfig{allEpisodes} = do
  atomicModify' $ \(FullChannels fullChannels) ->
    FullChannels $ if allEpisodes then S.insert cid fullChannels else S.delete cid fullChannels
  getFeedConfig cid
