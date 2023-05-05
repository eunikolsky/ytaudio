{-# LANGUAGE DataKinds #-}

module Lib
  ( startApp
  , app
  ) where

import Adapters.Service qualified as Ad
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve Ad.api Ad.server
