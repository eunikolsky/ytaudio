module Main (main) where

import Config (Config (..))
import Lib (startApp)
import Options.Generic (unwrapRecord)
import Usecases.AudioFeed qualified as UC (URLPrefix (..))

main :: IO ()
main = do
  Config{port, urlPrefix} <- unwrapRecord "youtube-audio-feed"
  startApp (port, UC.URLPrefix urlPrefix)
