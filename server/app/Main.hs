{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runNoLoggingT)
import           Data.Text           (Text)
import           Database.Persist
import           Database.Persist.Quasi
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Ogma.Api.Definition
import           Ogma.Model.Model
import           Servant
import qualified Data.Auth.Identity as Auth
import qualified Ogma.Api            as Api

ogmaProxy :: Proxy OgmaApi
ogmaProxy = Proxy

ogma :: Application
ogma = simpleCors $ serve ogmaProxy Api.hello

main :: IO ()
main = do pool <- runNoLoggingT $ createSqlitePool "db.sqlite" 10
          runSqlPool (runMigration Auth.migrateAuth) pool
          runSqlPool (runMigration migrateAll) pool
          runSqlPool (createNewUser "moi" "moi@m.oi") pool
          liftIO $ run 8080 ogma
