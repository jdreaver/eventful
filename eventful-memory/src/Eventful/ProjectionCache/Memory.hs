{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Eventful.ProjectionCache.Memory
  ( ProjectionMap
  , emptyProjectionMap
  , projectionMapTVar
  , tvarProjectionCache
  , embeddedStateProjectionCache
  , module Eventful.ProjectionCache.Types
  ) where

import Control.Concurrent.STM
import Control.Monad.State.Class hiding (state)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Eventful.ProjectionCache.Types

-- | A 'ProjectionMap' just stores the latest snapshot for each UUID.
type ProjectionMap key orderKey serialized = Map key (orderKey, serialized)

emptyProjectionMap :: ProjectionMap key orderKey serialized
emptyProjectionMap = Map.empty

projectionMapTVar :: IO (TVar (ProjectionMap key orderKey serialized))
projectionMapTVar = newTVarIO emptyProjectionMap

storeProjectionInMap
  :: (Ord key)
  => key
  -> orderKey
  -> serialized
  -> ProjectionMap key orderKey serialized
  -> ProjectionMap key orderKey serialized
storeProjectionInMap uuid version state = Map.insert uuid (version, state)

-- | A 'ProjectionCache' that uses a 'TVar' and runs in 'STM'.
tvarProjectionCache
  :: (Ord key)
  => TVar (ProjectionMap key orderKey serialized)
  -> ProjectionCache key orderKey serialized STM
tvarProjectionCache tvar =
  let
    storeProjectionSnapshot uuid version projState = modifyTVar' tvar (storeProjectionInMap uuid version projState)
    loadProjectionSnapshot uuid = Map.lookup uuid <$> readTVar tvar
  in ProjectionCache{..}

-- | A 'ProjectionCache' for some 'MonadState' that contains a 'ProjectionMap'.
embeddedStateProjectionCache
  :: (MonadState s m, Ord key)
  => (s -> ProjectionMap key orderKey serialized)
  -> (s -> ProjectionMap key orderKey serialized -> s)
  -> ProjectionCache key orderKey serialized m
embeddedStateProjectionCache getMap setMap =
  let
    storeProjectionSnapshot uuid version projState = modify' (storeProjectionSnapshot' uuid version projState)
    loadProjectionSnapshot uuid = Map.lookup uuid <$> gets getMap
  in ProjectionCache{..}
  where
    storeProjectionSnapshot' uuid version projState state =
      setMap state $ storeProjectionInMap uuid version projState $ getMap state
