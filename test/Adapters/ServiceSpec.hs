module Adapters.ServiceSpec (spec) where

import Adapters.Service
import Control.Concurrent.MVar
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Text.Lazy.Encoding qualified as TEL
import Domain.LiveStatus qualified as Dom
import Domain.YtDlpChannelStreams qualified as Dom
import External.Errors qualified as Ext
import Paths_ytaudio (getDataFileName)
import Polysemy
import Polysemy.AtomicState
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import Servant.Server
import Test.Hspec
import Test.Hspec.Wai
import Text.Show.Unicode
import URI.ByteString (Port (..))
import Usecases.AudioFeed qualified as UC
import Usecases.EncodeAudio qualified as UC
import Usecases.FeedConfig qualified as UC
import Usecases.GetFeedConfig qualified as UC
import Usecases.LiveStreamCheck qualified as UC
import Usecases.RunYoutubePure
import Usecases.Youtube qualified as UC

spec :: Spec
spec = with createApp $ do
  describe "GET /feed/<channelid>" $ do
    it "responds with audio RSS" $ do
      -- note: this file was generated by the test and manually confirmed that
      -- it looks good, so it's used as the expected file; changes in the RSS
      -- rendering may break this test even if the meaning stays the same!
      expected <- liftIO $ BSL.readFile "test/Adapters/expectedAudioFeed.rss"
      get "/feed/UCnExw5tVdA3TJeb4kmCd-JQ" `shouldRespondWith` bodyMatches expected

bodyMatches :: BSL.ByteString -> ResponseMatcher
bodyMatches expected =
  ResponseMatcher
    { -- this trailing `\n` is needed to make `expectedAudioFeed.rss` a good text
      -- file, and since the RSS renderer doesn't add it, we need to add it here
      matchBody = MatchBody $ \_headers body -> eqBody (body <> "\n")
    , matchStatus = 200
    , matchHeaders = []
    }
  where
    eqBody body =
      if body == expected
        then Nothing
        else
          Just $
            mconcat
              [ "expected body to be "
              , ushow (TEL.decodeUtf8 expected)
              , "\nbut got "
              , ushow (TEL.decodeUtf8 body)
              ]

createApp :: IO Application
createApp = do
  -- FIXME using the file from the `Domain/` directory
  youtubeFeed <- T.readFile =<< getDataFileName "test/Domain/videos.xml"
  concurrentLock <- newMVar ()
  let streams = Dom.Streams mempty
  pure $ serve api (liftServer concurrentLock streams youtubeFeed)

-- This is based on the code in
-- https://thma.github.io/posts/2020-05-29-polysemy-clean-architecture.html#testing-the-rest-api
liftServer :: MVar () -> Dom.Streams -> Text -> Server API
liftServer concurrentLock streams youtubeFeed = hoistServer api (interpretServer streams youtubeFeed) (server concurrentLock)

interpretServer
  :: Dom.Streams
  -> Text
  -> Sem
      [ UC.Youtube
      , UC.EncodeAudio
      , Error AudioServerError
      , Input (Port, UC.URLPrefix)
      , UC.LiveStreamCheck
      , AtomicState UC.FullChannels
      , AtomicState UC.AudioFeedItems
      , Resource
      , Embed IO
      ]
      x
  -> Handler x
interpretServer streams youtubeFeed =
  liftToHandler
    . runM
    . runResource
    . evalAtomicStateViaState @UC.AudioFeedItems mempty
    . evalAtomicStateViaState @UC.FullChannels mempty
    . runLiveStreamCheckPure
    . runInputConst (Port 8080, UC.URLPrefix "http://localhost:8080")
    . runError @AudioServerError
    . runEncodeAudioPure
    . runYoutubePure (UC.ChannelId "UCnExw5tVdA3TJeb4kmCd-JQ", youtubeFeed, streams)

liftToHandler :: IO (Either AudioServerError x) -> Handler x
liftToHandler = Handler . ExceptT . fmap (first Ext.handleErrors)

runEncodeAudioPure :: Sem (UC.EncodeAudio ': r) a -> Sem r a
runEncodeAudioPure = interpret $ \case
  UC.EncodeAudio _ -> error "FIXME implement for tests"

runLiveStreamCheckPure :: Sem (UC.LiveStreamCheck : r) a -> Sem r a
runLiveStreamCheckPure = interpret $ \case
  UC.LiveStreamCheck _ -> pure Dom.NotLive
