module Adapters.Service (api, server)
where

import Data.Text (Text)
import Servant

type API = Get '[PlainText] Text

api :: Proxy API
api = Proxy

server :: Server API
server = getRoot

getRoot :: Handler Text
getRoot = return "hello world"
