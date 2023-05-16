module SkipLiveStreamCheck (skipLiveStreamCheck) where

import Domain.LiveStatus qualified as Dom
import Polysemy
import Usecases.LiveStreamCheck qualified as UC

{- | `UC.LiveStreamCheck` interpeter that always returns `Dom.wasLive`, meaning
the stream can be downloaded.
-}
skipLiveStreamCheck :: InterpreterFor UC.LiveStreamCheck r
skipLiveStreamCheck = interpret $ \case
  UC.LiveStreamCheck _ -> pure Dom.WasLive
