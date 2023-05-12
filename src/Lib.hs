{-# LANGUAGE DataKinds #-}

module Lib
  ( startApp
  , app
  ) where

import Adapters.Service qualified as Ad
import Control.Concurrent.MVar
import Control.Monad.Except
import Data.Bifunctor (first)
import External.Errors qualified as Ext
import Network.Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Logger
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import RunEncodeAudioProcess
import RunLiveStreamCheckYtDlp
import RunYoutubeHTTP
import Servant.Server
import URI.ByteString (Port (..))
import Usecases.EncodeAudio qualified as UC
import Usecases.LiveStreamCheck qualified as UC
import Usecases.Youtube qualified as UC

startApp :: Warp.Port -> IO ()
startApp port = withStdoutLogger $ \aplogger -> do
  -- From: https://github.com/algas/haskell-servant-cookbook/blob/master/doc/Logger.md
  -- TODO this doesn't show the content size (because servant always uses chunked
  -- encoding); it would be great to count the bytes in the response and show
  -- that too
  let settings = Warp.setPort port $ Warp.setLogger aplogger Warp.defaultSettings
  concurrentLock <- newMVar ()
  Warp.runSettings settings $ app concurrentLock port

app :: MVar () -> Warp.Port -> Application
app concurrentLock port = serve Ad.api $ liftServer concurrentLock port

liftServer :: MVar () -> Warp.Port -> Server Ad.API
liftServer concurrentLock port = hoistServer Ad.api (interpretServer port) (Ad.server concurrentLock)

interpretServer
  :: Warp.Port
  -> Sem
      [ UC.Youtube
      , UC.EncodeAudio
      , Error Ad.AudioServerError
      , Input Port
      , UC.LiveStreamCheck
      , Resource
      , Embed IO
      ]
      a
  -> Handler a
interpretServer port =
  liftToHandler
    . runM
    . runResource
    . runLiveStreamCheckYtDlp
    . runInputConst (Port port)
    . runError @Ad.AudioServerError
    . runEncodeAudioProcess
    . runYoutubeHTTP

liftToHandler :: IO (Either Ad.AudioServerError x) -> Handler x
liftToHandler = Handler . ExceptT . fmap (first Ext.handleErrors)
