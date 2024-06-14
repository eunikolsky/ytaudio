module Config
  ( Config (..)
  ) where

import Data.ByteString (ByteString)
import Network.Wai.Handler.Warp (Port)
import Options.Generic (Generic, ParseRecord, Wrapped, (:::), type (<!>), type (<#>), type (<?>))

data Config w = Config
  { port :: w ::: Port <#> "p" <!> "8080" <?> "Port to listen on"
  , host :: w ::: ByteString <#> "h" <!> "localhost" <?> "Server's hostname to use in the feeds"
  }
  deriving stock (Generic)

instance ParseRecord (Config Wrapped)
