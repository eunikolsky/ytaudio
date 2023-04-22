{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    , parseYoutubeFeed
    ) where

import Conduit
--import Control.Concurrent
--import Data.Conduit.List qualified as C
import Data.Conduit.Process qualified as C
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Conduit ()
import System.Process
import Text.XML.Light

type API
  = "feed" :> Capture "channelid" Text :> Get '[PlainText] Text
  :<|> "yt" :> Capture "videoid" Text :> StreamGet NoFraming PlainText (ConduitT () Text IO ())

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = getFeed :<|> streamAudio

getFeed :: Text -> Handler Text
getFeed _channelId = liftIO $ T.readFile "samplefeed.rss"
  --xml <- liftIO $ T.readFile "ytsample.xml"
  --pure . T.pack . either id show . parseYoutubeFeed $ xml

-- shell command:
-- `yt-dlp -f ba --no-progress 'https://www.youtube.com/watch?v=id' -o - | \
-- ffmpeg -hide_banner -v warning -i pipe: -vn -acodec libmp3lame -b:a 96k \
--   -movflags +faststart -metadata title='foo bar' -metadata genre=Podcast -f mp3 pipe:`
--
-- From https://github.com/xxcodianxx/youtube-dl-web/blob/master/server/src/util/stream.py#L59-L68
-- and verbose output of `yt-dlp`
-- https://github.com/yt-dlp/yt-dlp/blob/master/yt_dlp/postprocessor/ffmpeg.py
streamAudio :: Text -> Handler (ConduitT () Text IO ())
streamAudio videoId =
  -- https://github.com/haskell-servant/servant/blob/master/servant-conduit/example/Main.hs
  let getURLsProc = proc "yt-dlp" ["-f", "ba", "--no-progress", "https://www.youtube.com/watch?v=" <> T.unpack videoId, "--simulate", "--print", "urls"]
  in liftIO $ do
    (C.ClosedStream,  out, C.Inherited, _phandle) <- C.streamingProcess getURLsProc
    pure $ out .| decodeUtf8C
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
