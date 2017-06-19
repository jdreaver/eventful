{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Cafe.DB
  ( openTab
  , getTabUuid
  , migrateTabEntity
  , TabEntity (..)
  , TabEntityId
  , Key (..)
  ) where

import           Control.Monad.IO.Class
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH

import           Eventful
import           Eventful.Store.Sqlite  ()

share [mkPersist sqlSettings, mkMigrate "migrateTabEntity"] [persistLowerCase|
TabEntity sql=tabs
    projectionId UUID
    deriving Show
|]

-- | Opens a tab by inserting an entry into the tabs table and returning the
-- UUID.
openTab :: (MonadIO m) => SqlPersistT m (TabEntityId, UUID)
openTab = do
  uuid <- liftIO uuidNextRandom
  key <- insert (TabEntity uuid)
  return (key, uuid)

-- | Given the tab id, attempts to load the tab and return the UUID.
getTabUuid :: (MonadIO m) => TabEntityId -> SqlPersistT m (Maybe UUID)
getTabUuid tabId = fmap tabEntityProjectionId <$> get tabId
