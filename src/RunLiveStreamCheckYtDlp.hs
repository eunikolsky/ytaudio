module RunLiveStreamCheckYtDlp (runLiveStreamCheckYtDlp) where

import Data.Char
import Data.List (dropWhileEnd)
import Data.Text qualified as T
import Domain.LiveStatus qualified as Dom
import Domain.YoutubeVideoId qualified as Dom
import Polysemy
import System.Process
import Usecases.LiveStreamCheck qualified as UC

runLiveStreamCheckYtDlp :: (Member (Embed IO) r) => InterpreterFor UC.LiveStreamCheck r
runLiveStreamCheckYtDlp = interpret $ \case
  UC.LiveStreamCheck videoId -> do
    let stdin = ""
    -- TODO check exit code manually
    liveStatusString <-
      embed $
        readCreateProcess
          (getLiveStatusProc videoId)
          stdin
    case Dom.liveStatusFromString (trim liveStatusString) of
      Just liveStatus -> pure liveStatus
      Nothing -> error $ "Could not parse live status: " <> liveStatusString

getLiveStatusProc :: Dom.YoutubeVideoId -> CreateProcess
getLiveStatusProc videoId =
  ( proc
      "yt-dlp"
      [ "--ignore-no-formats-error"
      , "-O"
      , "live_status"
      , "https://www.youtube.com/watch?v=" <> (T.unpack . Dom.getYoutubeVideoId $ videoId)
      ]
  )
    { -- ignore stderr as yt-dlp complains about unavailable formats, which
      -- is expected for upcoming live streams
      std_err = CreatePipe
    }

-- https://stackoverflow.com/questions/6270324/in-haskell-how-do-you-trim-whitespace-from-the-beginning-and-end-of-a-string/38283069#38283069
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
