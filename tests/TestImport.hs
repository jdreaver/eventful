-- | Common test functionality

module TestImport
  ( module X
  , Counter (..)
  , CounterEvent (..)
  , Command (..)
  , CommandError (..)
  ) where

import Control.Monad as X
import Control.Monad.IO.Class as X
import Control.Monad.Logger as X
import Database.Persist.Sqlite as X

import Data.Aeson
import Data.Aeson.TH

import EventSourcing

-- | Example Projection/Aggregate
newtype Counter = Counter { unCounter :: Int }
  deriving (Eq, Show, FromJSON, ToJSON)

data CounterEvent
  = Added
    { _counterEventAmount :: Int
    }
  deriving (Eq, Show)

instance Projection Counter where
  type Event Counter = CounterEvent
  seed = Counter 0
  apply (Counter k) (Added x) = Counter (k + x)

instance Aggregate Counter where
  data Command Counter
    = Increment
      { _counterCommandAmount :: Int
      }
    | Decrement
      { _counterCommandAmount :: Int
      }
    deriving (Eq, Show)

  data CommandError Counter
    = OutOfBounds
    deriving (Eq, Show)

  command (Counter k) (Increment n) =
    if k + n <= 100
    then Right $ Added n
    else Left OutOfBounds

  command (Counter k) (Decrement n) =
    if k - n >= 0
    then Right $ Added (-n)
    else Left OutOfBounds

deriveJSON defaultOptions ''CounterEvent
deriveJSON defaultOptions 'Increment
deriveJSON defaultOptions 'OutOfBounds
