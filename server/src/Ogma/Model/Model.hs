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
import Data.Auth.Token

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

getUserByIdentity :: (MonadBaseControl IO m, MonadIO m, Monad m)
                  => Auth.IdentityId
                  -> SqlPersistT m (Maybe (Entity User))
getUserByIdentity id = do getBy (UniqueIdentity id)

getUserByLogin :: (MonadBaseControl IO m, MonadIO m, Monad m)
               => Text
               -> SqlPersistT m (Maybe (Entity User))
getUserByLogin login = getBy (UniqueLogin login)

createUpdateDocPerm :: (MonadBaseControl IO m, MonadIO m, Monad m)
                    => UserId
                    -> DocumentId
                    -> Privilege
                    -> SqlPersistT m ()
createUpdateDocPerm userId docId perm = do
    deleteBy (UniqueDocUser docId userId) -- delete old perms
    insertUnique $ Permission docId userId perm

    return ()

createNewDocument :: (MonadBaseControl IO m, MonadIO m, Monad m)
                  => UserId
                  -> Text -- title
                  -> Text -- content
                  -> SqlPersistT m (DocumentId, Text)
createNewDocument id title content = do
    now <- liftIO $ getCurrentTime
    dir <- liftIO $ newToken64

    m <- insertUnique $ Document id (unToken dir) title now now 0

    (docId, dir) <- case m of Just id -> return (id, unToken dir)
                              _       -> createNewDocument id title content

    createUpdateDocPerm id docId Edit

    return (docId, dir)
