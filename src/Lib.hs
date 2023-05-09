{-# LANGUAGE DataKinds #-}

module Lib
  ( startApp
  , app
  ) where

import Adapters.Service qualified as Ad
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.ByteString.Lazy.Char8 qualified as BSLC
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TEL
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
import Usecases.AudioFeed qualified as UC
import Usecases.EncodeAudio qualified as UC
import Usecases.LiveStreamCheck qualified as UC
import Usecases.StreamAudio qualified as UC
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
liftToHandler = Handler . ExceptT . fmap (first handleErrors)

-- FIXME remove duplication with tests
handleErrors :: Ad.AudioServerError -> ServerError
handleErrors (Ad.DownloadAudioFeedError (UC.YoutubeFeedParseError text)) =
  err500{errBody = TEL.encodeUtf8 . TL.fromStrict $ text}
handleErrors (Ad.StreamAudioError (UC.LiveStreamNotReady status)) =
  Ad.err444{errBody = "Video can't be downloaded yet; live status: " <> BSLC.pack (show status)}
