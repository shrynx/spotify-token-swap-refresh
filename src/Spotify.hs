module Spotify
  ( tokenSwapRefreshApp
  , SpotifyConfig(..)
  )
where

import           Config                         ( SpotifyConfig(..)
                                                , AppM
                                                )
import           Data.ByteString.Base64         ( encode )
import           Network.HTTP.Client            ( newManager )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Relude
import           Servant                        ( (:<|>)(..)
                                                , Application
                                                , ServerT
                                                , err503
                                                , errBody
                                                , hoistServer
                                                , serve
                                                , throwError
                                                )
import           Servant.Client                 ( BaseUrl(..)
                                                , ClientM
                                                , Scheme(Https)
                                                , ServantError(..)
                                                , client
                                                , mkClientEnv
                                                , runClientM
                                                )
import           Spotify.API                    ( TokenSwapRefreshAPI
                                                , spotifyTokenAPI
                                                , tokenSwapRefreshAPI
                                                )
import           Spotify.Types                  ( Code(..)
                                                , RefreshData(..)
                                                , RefreshResponse(..)
                                                , RefreshToken(..)
                                                , SwapData(..)
                                                , SwapResponse(..)
                                                )
import           Utils                          ( byteToText
                                                , textToByte
                                                , stringToByte
                                                , stringToText
                                                )
import           Crypto.Simple.CTR              ( decrypt
                                                , encrypt
                                                )

spotifyApiBaseUrl :: BaseUrl
spotifyApiBaseUrl = BaseUrl { baseUrlScheme = Https
                            , baseUrlHost   = "accounts.spotify.com"
                            , baseUrlPort   = 443
                            , baseUrlPath   = "api"
                            }

contentType :: Text
contentType = "application/x-www-form-urlencoded"

makeAuthHeader :: String -> String -> Text
makeAuthHeader clientId clientSecret = "Basic " <> byteToText
  (encode (stringToByte clientId <> ":" <> stringToByte clientSecret))

execQuery :: ClientM a -> IO (Either ServantError a)
execQuery q = do
  manager' <- newManager tlsManagerSettings
  runClientM q (mkClientEnv manager' spotifyApiBaseUrl)

swapSpotifyToken :: Maybe Text -> Maybe Text -> SwapData -> ClientM SwapResponse
refreshSpotifyToken
  :: Maybe Text -> Maybe Text -> RefreshData -> ClientM RefreshResponse
(swapSpotifyToken :<|> refreshSpotifyToken) = client spotifyTokenAPI

server :: ServerT TokenSwapRefreshAPI AppM
server = swapApi :<|> refreshApi

swapApi :: Code -> AppM SwapResponse
swapApi reqCode = do
  appConfig <- ask
  let authHeader =
        makeAuthHeader (spClientId appConfig) (spClientSecret appConfig)
  let encryptWithKey = encrypt (stringToByte $ encSecret appConfig)
  res <- liftIO $ execQuery $ swapSpotifyToken
    (Just authHeader)
    (Just contentType)
    (SwapData (stringToText $ uriEndpoint appConfig) (getCode reqCode))
  case res of
    Left _ -> throwError $ err503 { errBody = "sorry, something went wrong" }
    Right result -> do
      updatedToken <- liftIO $ encryptWithKey $ textToByte $ refreshToken result
      return $ result { refreshToken = byteToText updatedToken }

refreshApi :: RefreshToken -> AppM RefreshResponse
refreshApi rt = do
  appConfig <- ask
  let authHeader =
        makeAuthHeader (spClientId appConfig) (spClientSecret appConfig)
  let decryptWithKey = decrypt (stringToByte $ encSecret appConfig)
  decryptedToken <- liftIO $ decryptWithKey $ textToByte $ getRefreshToken rt
  res            <- liftIO $ execQuery $ refreshSpotifyToken
    (Just authHeader)
    (Just contentType)
    (RefreshData $ byteToText decryptedToken)
  case res of
    Left _ -> throwError $ err503 { errBody = "sorry, something went wrong" }
    Right result -> return result

tokenSwapRefreshApp :: SpotifyConfig -> Application
tokenSwapRefreshApp config = serve tokenSwapRefreshAPI
  $ hoistServer tokenSwapRefreshAPI (`runReaderT` config) server
