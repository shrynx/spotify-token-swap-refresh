module Config
  ( SpotifyConfig(..)
  , AppM
  )
where
  
import           Relude
import           Servant                        ( Handler )

data SpotifyConfig = SpotifyConfig
  { uriEndpoint    :: String
  , encSecret      :: String
  , spClientId     :: String
  , spClientSecret :: String
  } deriving (Show)

type AppM = ReaderT SpotifyConfig Handler
