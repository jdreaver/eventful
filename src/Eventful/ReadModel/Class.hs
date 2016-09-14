module Eventful.ReadModel.Class
  ( ReadModel (..)
  , EventHandler (..)
  , combineHandlers
  ) where

import Control.Monad (mapM_)

import Eventful.Store

class (Monad m) => ReadModel m model serialized | model -> serialized where
  latestApplied :: model -> m SequenceNumber
  applyEvents :: model -> [StoredEvent serialized] -> m ()

data EventHandler m serialized = forall event. (Serializable event serialized, Monad m) => EventHandler (event -> m ())

combineHandlers :: (Monad m) => [EventHandler m serialized] -> (serialized -> m ())
combineHandlers handlers event = mapM_ ($ event) (mkHandler <$> handlers)

mkHandler :: EventHandler m serialized -> (serialized -> m ())
mkHandler (EventHandler handler) event = maybe (return ()) handler (deserialize event)
