module AppConfig where

import           Relude
import           Spotify                        ( SpotifyConfig(..) )
import           System.Envy                    ( FromEnv(..)
                                                , env
                                                )

data AppConfig = AppConfig
  { appPort :: Int
  , appSpotifyConfig :: SpotifyConfig
  } deriving (Show)

instance FromEnv AppConfig where
  fromEnv =
    AppConfig
      <$> env "PORT"
      <*> (   SpotifyConfig
          <$> env "URI_ENDPOINT"
          <*> env "ENC_SECRET"
          <*> env "SP_CLIENT_ID"
          <*> env "SP_CLIENT_SECRET"
          )
