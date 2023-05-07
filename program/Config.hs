module Config
  ( Config (..)
  ) where

import Network.Wai.Handler.Warp (Port)
import Options.Generic (Generic, ParseRecord, Wrapped, (:::), type (<!>), type (<#>), type (<?>))

newtype Config w = Config
  { port :: w ::: Port <#> "p" <!> "8080" <?> "Port to listen on"
  }
  deriving stock (Generic)

instance ParseRecord (Config Wrapped)
