{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    , parseYoutubeFeed
    ) where

import Conduit
--import Control.Concurrent
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
--import Data.Conduit.List qualified as C
import Data.Conduit.Process qualified as C
import Data.String (IsString)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.HTTP.Media ((//))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Conduit ()
import System.Process
import Text.XML.Light

data MP3

instance Accept MP3 where
  contentType _ = "audio" // "mpeg"

instance MimeRender MP3 ByteString where
  mimeRender _ = BS.fromStrict

instance MimeRender MP3 () where
  mimeRender _ = const BSL.empty

type Head = Verb 'HEAD 200

type API
  = "feed" :> Capture "channelid" Text :> Get '[PlainText] Text
  -- HEAD needs to be handled explicitly so that we don't start the subprocesses
  -- and not log errors from ffmpeg about "Broken pipe"
  :<|> "yt" :> Capture "videoid" Text :> Head '[MP3] (Headers '[Header "Content-Disposition" Text] ())
  :<|> "yt" :> Capture "videoid" Text :> StreamGet NoFraming MP3 (Headers '[Header "Content-Disposition" Text] (ConduitT () ByteString IO ()))

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = getFeed :<|> emptyStreamAudio :<|> streamAudio

getFeed :: Text -> Handler Text
getFeed _channelId = liftIO $ T.readFile "samplefeed.rss"
  --xml <- liftIO $ T.readFile "ytsample.xml"
  --pure . T.pack . either id show . parseYoutubeFeed $ xml

emptyStreamAudio :: Text -> Handler (Headers '[Header "Content-Disposition" Text] ())
emptyStreamAudio videoId = pure $ addFilenameHeader videoId ()

addFilenameHeader :: (AddHeader h v orig new, Semigroup v, IsString v) => v -> orig -> new
addFilenameHeader videoId = addHeader ("attachment; filename=\"" <> videoId <> ".mp3\"")

-- shell command:
-- `yt-dlp -f ba --no-progress 'https://www.youtube.com/watch?v=id' -o - | \
-- ffmpeg -hide_banner -v warning -i pipe: -vn -acodec libmp3lame -b:a 96k \
--   -movflags +faststart -metadata title='foo bar' -metadata genre=Podcast -f mp3 pipe:`
--
-- From https://github.com/xxcodianxx/youtube-dl-web/blob/master/server/src/util/stream.py#L59-L68
-- and verbose output of `yt-dlp`
-- https://github.com/yt-dlp/yt-dlp/blob/master/yt_dlp/postprocessor/ffmpeg.py
streamAudio :: Text -> Handler (Headers '[Header "Content-Disposition" Text] (ConduitT () ByteString IO ()))
streamAudio videoId =
  -- https://github.com/haskell-servant/servant/blob/master/servant-conduit/example/Main.hs
  let getBestAudioProc = proc "yt-dlp"
        ["-f", "ba", "--quiet", "https://www.youtube.com/watch?v=" <> T.unpack videoId, "-o", "-"]
      encodeToMP3Proc = proc "ffmpeg"
        ["-hide_banner", "-v", "warning", "-i", "pipe:", "-vn", "-acodec", "libmp3lame", "-b:a", "96k"
        , "-movflags", "+faststart", "-metadata", "genre=Podcast", "-f", "mp3", "pipe:"]
  in do
    (C.ClosedStream, bestAudioOut, C.Inherited, _) <- C.streamingProcess getBestAudioProc
    (C.UseProvidedHandle, encodedMP3Out, C.Inherited, _) <- C.streamingProcess encodeToMP3Proc { std_in = UseHandle bestAudioOut }
    pure $ addFilenameHeader videoId encodedMP3Out
    -- FIXME how to make sure the process terminates?
    --C.waitForStreamingProcess cph

data YTEntry = YTEntry
  { yteId :: !Text
  , yteTitle :: !Text
  }
  deriving Show

parseYoutubeFeed :: Text -> Either String [YTEntry]
parseYoutubeFeed = parseAtom . parseXML

parseAtom :: [Content] -> Either String [YTEntry]
parseAtom contents = do
  let elements = onlyElems contents
  feed <- find ((== atomQual "feed") . elName) elements <?> "no feed tag"
  entries <- ensure (not . null) (findElements (atomQual "entry") feed) <?> "no entries"
  traverse parseAtomEntry entries

parseAtomEntry :: Element -> Either String YTEntry
parseAtomEntry element = do
  yteId <- T.pack . strContent <$> findChild (qual "http://www.youtube.com/xml/schemas/2015" "videoId") element <?> "no videoId"
  yteTitle <- T.pack . strContent <$> findChild (atomQual "title") element <?> "no title"
  pure YTEntry { yteId, yteTitle }

atomQual :: String -> QName
atomQual = qual "http://www.w3.org/2005/Atom"

qual :: String -> String -> QName
qual uri name = QName { qName = name, qURI = Just uri, qPrefix = Nothing }

ensure :: (a -> Bool) -> a -> Maybe a
ensure p x = if p x then Just x else Nothing

-- infix 7 <?>
(<?>) :: Maybe a -> b -> Either b a
Just x <?> _ = Right x
Nothing <?> e = Left e
