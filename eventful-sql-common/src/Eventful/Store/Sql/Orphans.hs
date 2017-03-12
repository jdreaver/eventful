{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eventful.Store.Sql.Orphans
  (
  ) where

import Data.Proxy
import qualified Data.Text.Encoding as TE
import Data.UUID
import Database.Persist
import Database.Persist.Sql

import Eventful.Store.Class
import Eventful.UUID

instance PersistField UUID where
  toPersistValue = PersistText . uuidToText
  fromPersistValue (PersistDbSpecific t) =
    case uuidFromText (TE.decodeUtf8 t) of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue (PersistText t) =
    case uuidFromText t of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PersistField EventVersion where
  toPersistValue = toPersistValue . unEventVersion
  fromPersistValue = fmap EventVersion . fromPersistValue

instance PersistFieldSql EventVersion where
  sqlType _ = sqlType (Proxy :: Proxy Int)

instance PersistField SequenceNumber where
  toPersistValue = toPersistValue . unSequenceNumber
  fromPersistValue = fmap SequenceNumber . fromPersistValue

instance PersistFieldSql SequenceNumber where
  sqlType _ = sqlType (Proxy :: Proxy Int)
