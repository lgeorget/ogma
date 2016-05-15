{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

{-|
Module      : Ogma.Api.Definition 
Copyright   : (c) Ogma Project, 2016
License     : MIT
Stability   : experimental

-}

module Ogma.Api.Definition where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH
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
deriveJSON (defaultOptions { fieldLabelModifier = map toLower . drop 10 }) ''AccountNewPost

data GetTokenPost = GetTokenPost { getTokenLogin :: Text }
  deriving (Generic)
deriveJSON (defaultOptions { fieldLabelModifier = map toLower . drop 8 }) ''GetTokenPost

data GetTokenResponse = GetTokenResponse { getTokenAccess  :: Text
                                         , getTokenRefresh :: Text
                                         , getTokenExpire  :: UTCTime }
  deriving (Generic)
deriveJSON (defaultOptions { fieldLabelModifier = map toLower . drop 8 }) ''GetTokenResponse

data DocumentPost = DocumentPost { postDocumentTitle   :: Text
                                 , postDocumentContent :: Text }
  deriving (Generic)
deriveJSON (defaultOptions { fieldLabelModifier = map toLower . drop 12 }) ''DocumentPost

data GetDocument = GetDocument { getDocumentTitle      :: Text
                               , getDocumentContent    :: Text
                               , getDocumentModifiedOn :: UTCTime
                               , getDocumentCreatedOn  :: UTCTime
                               , getDocumentPerm       :: Text }
  deriving (Generic)
deriveJSON (defaultOptions { fieldLabelModifier = map toLower . drop 11 }) ''GetDocument

type OgmaAPI = "account"
                 :> "new"
                 :> ReqBody '[JSON] AccountNewPost
                 :> PostCreated '[JSON] (Headers '[Header "resource-id" Int64] NoContent)
          :<|> "get_token"
                 :> ReqBody '[JSON] GetTokenPost
                 :> Post '[JSON] GetTokenResponse
          :<|> AuthProtect "ogma-identity" :>
                ("document"
                   :> "new"
                   :> ReqBody '[JSON] DocumentPost
                   :> PostCreated '[JSON] (Headers '[Header "resource-id" Int64] NoContent)
            :<|> "document"
                   :> Capture "id" Int64
                   :> ReqBody '[JSON] DocumentPost
                   :> Put '[JSON] NoContent
            :<|> "document"
                   :> Capture "id" Int64
                   :> Get '[JSON] GetDocument)

ogmaAPI :: Proxy OgmaAPI
ogmaAPI = Proxy
