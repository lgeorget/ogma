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

import           GHC.Generics
import           Data.Text   (Text)
import           Servant.API
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Proxy
import Data.Char (toLower)

data AccountNewPost = AccountNewPost { accountNewEmail :: Text
                                     , accountNewLogin :: Text }
  deriving (Generic)

instance ToJSON AccountNewPost where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = map toLower . drop 10 }

instance FromJSON AccountNewPost where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 10 }

type OgmaAPI = "account" :> "new" :> ReqBody '[JSON] AccountNewPost :> PostCreated '[JSON] ()

ogmaAPI :: Proxy OgmaAPI
ogmaAPI = Proxy
