module Main (main) where

import Config (port)
import Lib (startApp)
import Options.Generic (unwrapRecord)

main :: IO ()
main = do
  config <- unwrapRecord "youtube-audio-feed"
  startApp $ port config
