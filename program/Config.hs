module Config
  ( Config (..)
  ) where

import Data.Text (Text)
import Network.Wai.Handler.Warp (Port)
import Options.Generic
  ( Generic
  , ParseRecord (..)
  , Wrapped
  , lispCaseModifiers
  , parseRecordWithModifiers
  , (:::)
  , type (<!>)
  , type (<#>)
  , type (<?>)
  )

data Config w = Config
  { port :: w ::: Port <#> "p" <!> "8080" <?> "Port to listen on"
  , urlPrefix
      :: w ::: Text <#> "u" <!> "http://localhost:8080" <?> "Server's public URL prefix to use in the feeds"
  }
  deriving stock (Generic)

instance ParseRecord (Config Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers
