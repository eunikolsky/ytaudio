{-# LANGUAGE DataKinds #-}

module Lib
  ( startApp
  , app
  ) where

import Network.Wai
import Network.Wai.Handler.Warp qualified as Warp

startApp :: IO ()
startApp = Warp.run 8080 app

app :: Application
-- FIXME implement!
app = undefined
