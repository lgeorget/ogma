{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import qualified Ogma.Api            as Api
import           Ogma.Api.Definition
import           Ogma.Model.Model
import           Data.Text           (Text)
import           Servant
import           Network.Wai
import           Network.Wai.Middleware.Cors
import           Network.Wai.Handler.Warp
import           Database.Persist
import           Database.Persist.TH
import           Database.Persist.Quasi
import           Database.Persist.Sqlite
import qualified Data.Auth.Identity as Auth

ogmaProxy :: Proxy OgmaApi
ogmaProxy = Proxy

ogma :: Application
ogma = simpleCors $ serve ogmaProxy Api.hello

main :: IO ()
main = do runSqlite "db.sqlite" $ do
            runMigration migrateAll
            runMigration Auth.migrateAuth
            createNewUser "moi" "moi@m.oi"
          liftIO $ run 8080 ogma
