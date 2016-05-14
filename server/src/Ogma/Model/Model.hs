{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}

module Ogma.Model.Model where

import Database.Persist
import Database.Persist.Quasi
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time
import Data.Text               (Text)
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import qualified Data.Auth.Identity as Auth
import Ogma.Model.Privilege

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "models/db.model")

createNewUser :: (MonadBaseControl IO m, MonadIO m, Monad m)
              => Text
              -> Text
              -> SqlPersistT m (Maybe UserId)
createNewUser login email = do
    identity <- Auth.newIdentity
    insertUnique $ User login identity email
