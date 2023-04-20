{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

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
getFeed ytid = pure $ "requested feed for youtube channel " <> ytid
