{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Ogma.Server where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import Database.Persist.Sql
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Servant
import Data.Text (Text)
import Data.Time

import Ogma.Api.Definition
import Data.Auth.Token
import Data.Auth.Identity
import Auth.Identity.Servant

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

accountNewH :: AccountNewPost -> OgmaM ()
accountNewH _ = throwError err400

server :: ServerT OgmaAPI OgmaM
server = accountNewH

readerToExcept :: OgmaConfig
               -> OgmaM
               :~> ExceptT ServantErr IO
readerToExcept cfg = Nat $ \x -> runReaderT x cfg

readerServer :: OgmaConfig
             -> Server OgmaAPI
readerServer cfg = enter (readerToExcept cfg) server

ogmad :: OgmaConfig
      -> Application
ogmad cfg = logStdout $ serveWithContext ogmaAPI (authIdentityServerContext $ getPool cfg) (readerServer cfg)
