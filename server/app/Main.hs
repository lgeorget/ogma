{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runNoLoggingT)
import           Data.Auth.Token
import           Data.Text                   (Text)
import           Database.Persist
import           Database.Persist.Quasi
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Ogma.Api.Definition
import           Ogma.Model.Model
import           Ogma.Server
import           Servant

import qualified Data.Auth.Identity          as Auth
import qualified Ogma.Api                    as Api

main :: IO ()
main = do pool <- runNoLoggingT $ createSqlitePool "db.sqlite" 10
          runSqlPool (runMigration Auth.migrateAuth) pool
          runSqlPool (runMigration migrateAll) pool

          let cfg :: OgmaConfig
              cfg = OgmaConfig pool
                               Token64
                               Token256
                               120
                               3600

          run 8080 (simpleCors $ ogmad cfg)
