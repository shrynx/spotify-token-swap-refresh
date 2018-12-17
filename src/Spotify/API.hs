{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Spotify.API
  ( tokenSwapRefreshAPI
  , TokenSwapRefreshAPI
  , spotifyTokenAPI
  , SpotifyTokenAPI
  )
where
import           Data.Proxy                     ( Proxy )
import           Servant                        ( (:<|>)
                                                , (:>)
                                                , FormUrlEncoded
                                                , Header
                                                , JSON
                                                , Post
                                                , ReqBody
                                                )

import           Spotify.Types                  ( Code(..)
                                                , RefreshData(..)
                                                , RefreshResponse(..)
                                                , RefreshToken(..)
                                                , SwapData(..)
                                                , SwapResponse(..)
                                                )

import           Relude

type TokenSwapRefreshAPI
      = "swap"
      :> ReqBody '[FormUrlEncoded] Code
      :> Post '[JSON] SwapResponse
   :<|> "refresh"
      :> ReqBody '[FormUrlEncoded] RefreshToken
      :> Post '[JSON] RefreshResponse

type SpotifyTokenAPI
      = "token" 
      :> Header "Authorization" Text 
      :> Header "Content-Type" Text 
      :> ReqBody '[FormUrlEncoded] SwapData
      :> Post '[JSON] SwapResponse
   :<|> "token"
      :> Header "Authorization" Text 
      :> Header "Content-Type" Text 
      :> ReqBody '[FormUrlEncoded] RefreshData
      :> Post '[JSON] RefreshResponse

tokenSwapRefreshAPI :: Proxy TokenSwapRefreshAPI
tokenSwapRefreshAPI = Proxy

spotifyTokenAPI :: Proxy SpotifyTokenAPI
spotifyTokenAPI = Proxy
