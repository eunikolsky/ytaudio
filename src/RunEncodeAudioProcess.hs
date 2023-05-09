module RunEncodeAudioProcess (runEncodeAudioProcess) where

import Data.Conduit.Process qualified as C
import Data.Text qualified as T
import Domain.AudioFeed.Item qualified as Dom
import Polysemy
import System.Process
import Usecases.EncodeAudio qualified as UC

-- Example of using a `Conduit` to stream response data (with `ResourceT`)
-- https://github.com/haskell-servant/servant/blob/master/servant-conduit/example/Main.hs

runEncodeAudioProcess :: (Member (Embed IO) r) => Sem (UC.EncodeAudio ': r) a -> Sem r a
runEncodeAudioProcess = interpret $ \case
  UC.EncodeAudio videoId -> do
    -- TODO kill the processes if the client closes the connection early
    (C.ClosedStream, bestAudioOut, C.Inherited, _) <- C.streamingProcess $ getBestAudioProc videoId
    (C.UseProvidedHandle, encodedMP3Out, C.Inherited, _) <-
      C.streamingProcess encodeToMP3Proc{std_in = UseHandle bestAudioOut}
    pure encodedMP3Out

-- shell command:
-- `yt-dlp -f ba --quiet 'https://www.youtube.com/watch?v=id' -o - | \
-- ffmpeg -hide_banner -v warning -i pipe: -vn -acodec libmp3lame -b:a 96k \
--   -movflags +faststart -metadata title='foo bar' -metadata genre=Podcast -f mp3 pipe:`
--
-- From https://github.com/xxcodianxx/youtube-dl-web/blob/master/server/src/util/stream.py#L59-L68
-- and verbose output of `yt-dlp`
-- https://github.com/yt-dlp/yt-dlp/blob/master/yt_dlp/postprocessor/ffmpeg.py

getBestAudioProc :: Dom.YoutubeVideoId -> CreateProcess
getBestAudioProc videoId =
  proc
    "yt-dlp"
    [ "-f"
    , "ba"
    , "--quiet"
    , "https://www.youtube.com/watch?v=" <> (T.unpack . Dom.getYoutubeVideoId $ videoId)
    , "-o"
    , "-"
    ]

encodeToMP3Proc :: CreateProcess
encodeToMP3Proc =
  proc
    "ffmpeg"
    [ "-hide_banner"
    , "-v"
    , "warning"
    , "-i"
    , "pipe:"
    , "-vn"
    , "-acodec"
    , "libmp3lame"
    , "-b:a"
    , "96k"
    , "-movflags"
    , "+faststart"
    , "-metadata"
    , "genre=Podcast"
    , "-f"
    , "mp3"
    , "pipe:"
    ]
