{-# LANGUAGE DataKinds #-}

module Lib
  ( startApp
  , app
  ) where

import Adapters.Service qualified as Ad
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TEL
import Network.Wai
import Network.Wai.Handler.Warp qualified as Warp
import Polysemy
import Polysemy.Error
import RunYoutubeHTTP
import Servant.Server
import Usecases.AudioFeed qualified as UC
import Usecases.Youtube qualified as UC

startApp :: IO ()
startApp = Warp.run 8080 app

app :: Application
app = serve Ad.api liftServer

liftServer :: Server Ad.API
liftServer = hoistServer Ad.api interpretServer Ad.server

interpretServer :: Sem [UC.Youtube, Error UC.DownloadAudioFeedError, Embed IO] a -> Handler a
interpretServer =
  liftToHandler
    . runM
    . runError @UC.DownloadAudioFeedError
    . runYoutubeHTTP

liftToHandler :: IO (Either UC.DownloadAudioFeedError x) -> Handler x
liftToHandler = Handler . ExceptT . fmap (first handleErrors)

-- FIXME remove duplication with tests
handleErrors :: UC.DownloadAudioFeedError -> ServerError
handleErrors (UC.YoutubeFeedParseError text) = err500{errBody = TEL.encodeUtf8 . TL.fromStrict $ text}
