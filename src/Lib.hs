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
import Polysemy.Input
import RunYoutubeHTTP
import Servant.Server
import URI.ByteString (Port (..))
import Usecases.AudioFeed qualified as UC
import Usecases.Youtube qualified as UC

startApp :: Warp.Port -> IO ()
startApp port = Warp.run port $ app port

app :: Warp.Port -> Application
app = serve Ad.api . liftServer

liftServer :: Warp.Port -> Server Ad.API
liftServer port = hoistServer Ad.api (interpretServer port) Ad.server

interpretServer
  :: Warp.Port -> Sem [UC.Youtube, Error UC.DownloadAudioFeedError, Input Port, Embed IO] a -> Handler a
interpretServer port =
  liftToHandler
    . runM
    . runInputConst (Port port)
    . runError @UC.DownloadAudioFeedError
    . runYoutubeHTTP

liftToHandler :: IO (Either UC.DownloadAudioFeedError x) -> Handler x
liftToHandler = Handler . ExceptT . fmap (first handleErrors)

-- FIXME remove duplication with tests
handleErrors :: UC.DownloadAudioFeedError -> ServerError
handleErrors (UC.YoutubeFeedParseError text) = err500{errBody = TEL.encodeUtf8 . TL.fromStrict $ text}
