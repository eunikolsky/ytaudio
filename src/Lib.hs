{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    , parseYoutubeFeed
    ) where

import Control.Monad.IO.Class
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Text.XML.Light

type API = "feed" :> Capture "ytid" Text :> Get '[PlainText] Text

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = getFeed

getFeed :: Text -> Handler Text
getFeed _ytid = liftIO $ T.readFile "samplefeed.rss"
  --xml <- liftIO $ T.readFile "ytsample.xml"
  --pure . T.pack . either id show . parseYoutubeFeed $ xml

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
