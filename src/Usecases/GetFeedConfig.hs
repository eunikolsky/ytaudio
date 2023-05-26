module Usecases.GetFeedConfig (getFeedConfig) where

import Polysemy
import Usecases.FeedConfig

getFeedConfig :: ChannelId -> Sem r FeedConfig
getFeedConfig cid = pure FeedConfig{channelId = cid, allEpisodes = False}
