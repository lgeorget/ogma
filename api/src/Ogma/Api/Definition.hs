{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Ogma.Api.Definition 
Copyright   : (c) Ogma Project, 2016
License     : MIT
Stability   : experimental

-}

module Ogma.Api.Definition where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Proxy
import           Data.Text   (Text)
import           Data.Time
import           GHC.Generics
import           GHC.Int
import           Servant.API
import           Data.Char (toLower)

data AccountNewPost = AccountNewPost { accountNewEmail :: Text
                                     , accountNewLogin :: Text }
  deriving (Generic)

instance ToJSON AccountNewPost where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = map toLower . drop 10 }
instance FromJSON AccountNewPost where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 10 }

data GetTokenPost = GetTokenPost { getTokenLogin :: Text }
  deriving (Generic)

instance ToJSON GetTokenPost where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = map toLower . drop 8 }
instance FromJSON GetTokenPost where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 8 }

data GetTokenResponse = GetTokenResponse { getTokenAccess  :: Text
                                         , getTokenRefresh :: Text
                                         , getTokenExpire  :: UTCTime }
  deriving (Generic)

instance ToJSON GetTokenResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = map toLower . drop 8 }
instance FromJSON GetTokenResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 8 }

type OgmaAPI = "account"
                 :> "new"
                 :> ReqBody '[JSON] AccountNewPost
                 :> PostCreated '[JSON] (Headers '[Header "resource-id" Int64] NoContent)
          :<|> "get_token"
                 :> ReqBody '[JSON] GetTokenPost
                 :> Post '[JSON] GetTokenResponse

ogmaAPI :: Proxy OgmaAPI
ogmaAPI = Proxy
