{-# LANGUAGE QuasiQuotes #-}

module Domain.YtDlpChannelStreamsSpec (spec) where

import Data.ByteString.Lazy qualified as LBS (ByteString)
import Data.Map qualified as M
import Domain.LiveStatus
import Domain.YoutubeVideoId
import Domain.YtDlpChannelStreams
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses channel streams lines" $ do
      parse input `shouldBe` expectedStreams

{- | Sample output from
`yt-dlp -f ba -O '%(.{id,live_status})+j' -I :3 --verbose --ignore-no-formats-error --no-check-formats https://www.youtube.com/channel/id`
-}
input :: LBS.ByteString
input =
  [r|{"id": "id0", "live_status": "is_upcoming"}
{"id": "e-89vyt", "live_status": "was_live"}
|]

expectedStreams :: Streams
expectedStreams =
  Streams . M.fromList $
    [ (YoutubeVideoId "id0", IsUpcoming)
    , (YoutubeVideoId "e-89vyt", WasLive)
    ]
