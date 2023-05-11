module External.Errors (handleErrors) where

import Adapters.Service qualified as Ad
import Data.ByteString.Lazy.Char8 qualified as BSLC
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TEL
import Servant.Server
import Usecases.AudioFeed qualified as UC
import Usecases.StreamAudio qualified as UC

{- | Transforms Adapters-level (and thus, internally, Usecases-level) errors
into External-level (servant) errors. This function is shared by the "real"
and test servant servers.
-}
handleErrors :: Ad.AudioServerError -> ServerError
handleErrors (Ad.DownloadAudioFeedError (UC.YoutubeFeedParseError text)) =
  err500{errBody = TEL.encodeUtf8 . TL.fromStrict $ text}
handleErrors (Ad.StreamAudioError (UC.LiveStreamNotReady status)) =
  Ad.err444NoResponse
    { errBody = "Video can't be downloaded yet; live status: " <> BSLC.pack (show status)
    }
