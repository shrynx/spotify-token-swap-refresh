module Main where

import           Network.Wai.Handler.Warp       ( run )
import           Relude
import           Spotify                        ( tokenSwapRefreshApp )
import           System.Envy                    ( decodeEnv )
import           AppConfig                      ( AppConfig(..) )

main :: IO ()
main = do
  appConfig <- decodeEnv :: IO (Either String AppConfig)
  case appConfig of
    Left  err    -> print ("Missing environment variable" :: Text) <> print err
    Right config -> run port $ tokenSwapRefreshApp spotifyConfig
                    where
                      port          = appPort config
                      spotifyConfig = appSpotifyConfig config
