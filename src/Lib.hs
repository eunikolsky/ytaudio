{-# LANGUAGE DataKinds #-}

module Lib
  ( startApp
  , app
  ) where

import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type API = Get '[PlainText] Text

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = getRoot

getRoot :: Handler Text
getRoot = return "hello world"
