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
import SkipLiveStreamCheck
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
    -- note the separation of concerns: the usecase doesn't need to change, and
    -- the outer layer decides whether to use the live stream check
    . (if useLiveStreamCheck then runLiveStreamCheckYtDlp else skipLiveStreamCheck)
    . runInputConst (Port port)
    . runError @Ad.AudioServerError
    . runEncodeAudioProcess
    . runYoutubeHTTP

liftToHandler :: IO (Either Ad.AudioServerError x) -> Handler x
liftToHandler = Handler . ExceptT . fmap (first Ext.handleErrors)

{- | The live stream check before downloading each mp3 is disabled because it
slows down the response and the generated audio feed doesn't contain upcoming
streams.

The user may still use the audio endpoint directly w/o the feed, which isn't
the main usecase. To separate these two cases, the feed should produce signed
URLs. More information:
https://blog.ploeh.dk/2020/10/26/fit-urls/#41cdd42bac1b4bd5a573070ee27e902d

TODO enable live stream check for unsigned URLs
-}
useLiveStreamCheck :: Bool
useLiveStreamCheck = False
