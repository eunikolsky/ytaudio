module Main (main) where

import Config (Config (..))
import Lib (startApp)
import Options.Generic (unwrapRecord)
import URI.ByteString (Host (..))

main :: IO ()
main = do
  Config{port, host} <- unwrapRecord "youtube-audio-feed"
  startApp (Host host, port)
