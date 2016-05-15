{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Ogma.Server where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Database.Persist.Sql
import GHC.Int
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Servant
import Servant.Server.Experimental.Auth

import Auth.Identity.Servant
import Data.Auth.Identity
import Data.Auth.Token
import Ogma.Api.Definition
import Ogma.Model.Model

data OgmaConfig = OgmaConfig { getPool       :: ConnectionPool
                             , accessSize    :: TokenSize
                             , refreshSize   :: TokenSize
                             , accessExpire  :: NominalDiffTime
                             , refreshExpire :: NominalDiffTime }

type OgmaM = ReaderT OgmaConfig (ExceptT ServantErr IO)

queryDb :: SqlPersistT OgmaM a
        -> OgmaM a
queryDb backReq = do pool <- getPool <$> ask
                     runSqlPool backReq pool

accountNewH :: AccountNewPost -> OgmaM (Headers '[Header "resource-id" Int64] NoContent)
accountNewH (AccountNewPost email login) = do
    userMaybe <- queryDb $ createNewUser login email

    case userMaybe of Just id -> return $ addHeader (fromSqlKey id) NoContent
                      Nothing -> throwError $ err403 { errBody = "Login or email have already been taken" }

authTokenToGetToken :: AuthToken -> GetTokenResponse
authTokenToGetToken (AuthToken ac re exAc _ _) = GetTokenResponse (unToken ac) (unToken re) exAc

getTokenH :: GetTokenPost
          -> OgmaM GetTokenResponse
getTokenH (GetTokenPost login) = do
    logMaybe <- queryDb $ getUserByLogin login

    case logMaybe of Just (Entity _ usr) -> do
                       acSize <- accessSize <$> ask
                       reSize <- refreshSize <$> ask
                       acExp <- accessExpire <$> ask
                       reExp <- refreshExpire <$> ask

                       authTok <- queryDb $ regenTokens (userIdentity usr) acExp reExp acSize reSize
                       return $ authTokenToGetToken authTok
                     Nothing -> throwError $ err403 { errBody = "Login unknown" }

newDocumentH :: (Entity User)
             -> DocumentPost
             -> OgmaM (Headers '[Header "resource-id" Int64] NoContent)
newDocumentH _ _ = throwError err400

putDocumentH :: (Entity User)
             -> Int64
             -> DocumentPost
             -> OgmaM NoContent
putDocumentH _ _ _ = throwError err400

getDocumentH :: (Entity User)
             -> Int64
             -> OgmaM GetDocument
getDocumentH _ _ = throwError err400

server :: ServerT OgmaAPI OgmaM
server = accountNewH
    :<|> getTokenH
    :<|> (\user -> (newDocumentH user
               :<|> putDocumentH user
               :<|> getDocumentH user))

readerToExcept :: OgmaConfig
               -> OgmaM
               :~> ExceptT ServantErr IO
readerToExcept cfg = Nat $ \x -> runReaderT x cfg

readerServer :: OgmaConfig
             -> Server OgmaAPI
readerServer cfg = enter (readerToExcept cfg) server

ogmaIdentityHandler :: ConnectionPool
                    -> IdentityId
                    -> Handler (Entity User)
ogmaIdentityHandler pool id = do
    userMaybe <- liftIO $ runSqlPool (getUserByIdentity id) pool

    case userMaybe of Just val -> return val
                      Nothing  -> throwError (err500 { errBody = "Orphan token" })

type instance AuthServerData (AuthProtect "ogma-identity") = Entity User

ogmad :: OgmaConfig
      -> Application
ogmad cfg =
    let pool = getPool cfg
    in logStdout $ serveWithContext ogmaAPI
                                    (authIdentityServerContext pool (ogmaIdentityHandler pool))
                                    (readerServer cfg)
