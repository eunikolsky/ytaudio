{-# LANGUAGE DataKinds #-}

module Lib
  ( startApp
  , app
  ) where

import Adapters.Service qualified as Ad
import Control.Monad.Except
import Data.Bifunctor (first)
import External.Errors qualified as Ext
import Network.Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Logger
import Polysemy
import Polysemy.Error
import Polysemy.Input
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
  Warp.runSettings settings $ app port

app :: Warp.Port -> Application
app = serve Ad.api . liftServer

liftServer :: Warp.Port -> Server Ad.API
liftServer port = hoistServer Ad.api (interpretServer port) Ad.server

interpretServer
  :: Warp.Port
  -> Sem
      [UC.Youtube, UC.EncodeAudio, Error Ad.AudioServerError, Input Port, UC.LiveStreamCheck, Embed IO]
      a
  -> Handler a
interpretServer port =
  liftToHandler
    . runM
    . runLiveStreamCheckYtDlp
    . runInputConst (Port port)
    . runError @Ad.AudioServerError
    . runEncodeAudioProcess
    . runYoutubeHTTP

liftToHandler :: IO (Either Ad.AudioServerError x) -> Handler x
liftToHandler = Handler . ExceptT . fmap (first Ext.handleErrors)
