{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedLists            #-}

module Spotify.Types
  ( Code(..)
  , RefreshData(..)
  , RefreshResponse(..)
  , RefreshToken(..)
  , SwapData(..)
  , SwapResponse(..)
  )
where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , genericParseJSON
                                                , genericToJSON
                                                )
import           Data.Aeson.Casing              ( aesonDrop
                                                , snakeCase
                                                )
import           GHC.Generics                   ( Generic )
import           Relude
import           Web.FormUrlEncoded             ( FromForm(..)
                                                , ToForm(..)
                                                , parseUnique
                                                )

newtype Code = Code
  { getCode :: Text
  } deriving (Show, Generic)

instance FromForm Code where
  fromForm = (Code <$>) . parseUnique "code"

newtype RefreshToken = RefreshToken
  { getRefreshToken :: Text
  } deriving (Show, Generic)

instance FromForm RefreshToken where
  fromForm = (RefreshToken <$>) . parseUnique "refresh_token"

data SwapData = SwapData {
  redirectUri :: Text
, code        :: Text
} deriving (Show)

instance ToForm SwapData where
  toForm sd =
    [ ("grant_type"  , "authorization_code")
    , ("redirect_uri", redirectUri sd)
    , ("code"        , code sd)
    ]

newtype RefreshData = RefreshData {
  refreshTokenData :: Text
} deriving (Show)

instance ToForm RefreshData where
  toForm rd =
    [("grant_type", "refresh_token"), ("refresh_token", refreshTokenData rd)]

data SwapResponse = SwapResponse {
  accessToken  :: Text
, expiresIn    :: Int
, refreshToken :: Text
} deriving (Show, Generic)

instance ToJSON SwapResponse where
  toJSON = genericToJSON $ aesonDrop 0 snakeCase

instance FromJSON SwapResponse where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

data RefreshResponse = RefreshResponse {
  accessToken :: Text
, expiresIn   :: Int
} deriving (Show, Generic)

instance ToJSON RefreshResponse where
  toJSON = genericToJSON $ aesonDrop 0 snakeCase

instance FromJSON RefreshResponse where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase
