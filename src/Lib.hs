{-# LANGUAGE DataKinds #-}

module Lib
  ( startApp
  , app
  ) where

import Adapters.Service qualified as Ad
import Control.Concurrent.MVar
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.IORef
import External.Errors qualified as Ext
import Network.Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Logger
import Polysemy
import Polysemy.AtomicState
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import RunEncodeAudioProcess
import RunLiveStreamCheckYtDlp
import RunYoutubeHTTP
import Servant.Server
import SkipLiveStreamCheck
import URI.ByteString (Port (..))
import Usecases.AudioFeed qualified as UC
import Usecases.EncodeAudio qualified as UC
import Usecases.GetFeedConfig qualified as UC
import Usecases.LiveStreamCheck qualified as UC
import Usecases.Youtube qualified as UC

startApp :: (Warp.Port, UC.URLPrefix) -> IO ()
startApp (port, urlPrefix) = withStdoutLogger $ \aplogger -> do
  -- From: https://github.com/algas/haskell-servant-cookbook/blob/master/doc/Logger.md
  -- TODO this doesn't show the content size (because servant always uses chunked
  -- encoding); it would be great to count the bytes in the response and show
  -- that too
  let settings = Warp.setPort port $ Warp.setLogger aplogger Warp.defaultSettings
  concurrentLock <- newMVar ()
  fullChannelsRef <- newIORef mempty
  feedItemsHistoryRef <- newIORef mempty
  Warp.runSettings settings $ app concurrentLock (port, urlPrefix) fullChannelsRef feedItemsHistoryRef

app
  :: MVar ()
  -> (Warp.Port, UC.URLPrefix)
  -> IORef UC.FullChannels
  -> IORef UC.AudioFeedItems
  -> Application
app concurrentLock (port, urlPrefix) fullChannelsRef feedItemsHistoryRef = serve Ad.api $ liftServer concurrentLock (port, urlPrefix) fullChannelsRef feedItemsHistoryRef

liftServer
  :: MVar ()
  -> (Warp.Port, UC.URLPrefix)
  -> IORef UC.FullChannels
  -> IORef UC.AudioFeedItems
  -> Server Ad.API
liftServer concurrentLock (port, urlPrefix) fullChannelsRef feedItemsHistoryRef =
  hoistServer
    Ad.api
    (interpretServer (port, urlPrefix) fullChannelsRef feedItemsHistoryRef)
    (Ad.server concurrentLock)

interpretServer
  :: (Warp.Port, UC.URLPrefix)
  -> IORef UC.FullChannels
  -> IORef UC.AudioFeedItems
  -> Sem
      [ UC.Youtube
      , UC.EncodeAudio
      , Error Ad.AudioServerError
      , Input (Port, UC.URLPrefix)
      , UC.LiveStreamCheck
      , AtomicState UC.FullChannels
      , AtomicState UC.AudioFeedItems
      , Resource
      , Embed IO
      ]
      a
  -> Handler a
interpretServer (port, urlPrefix) fullChannelsRef feedItemsHistoryRef =
  liftToHandler
    . runM
    . runResource
    . runAtomicStateIORef feedItemsHistoryRef
    -- note: `atomicStateToIO` does not work here because apparently it loses
    -- state after each request
    . runAtomicStateIORef fullChannelsRef
    -- note the separation of concerns: the usecase doesn't need to change, and
    -- the outer layer decides whether to use the live stream check
    . (if useLiveStreamCheck then runLiveStreamCheckYtDlp else skipLiveStreamCheck)
    . runInputConst (Port port, urlPrefix)
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
