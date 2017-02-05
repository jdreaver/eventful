module Cafe.Models.Tab
  ( TabState (..)
  , Drink (..)
  , Food (..)
  , OrderedItem (..)
  , TabProjection
  , tabProjection
  , TabAggregate
  , tabAggregate
  ) where

import Control.Lens
import Data.List (foldl')
import Data.Maybe (catMaybes)

import Eventful

data TabState =
  TabState
  { _tabStateIsOpen :: Bool
  , _tabStateOutstandingDrinks :: [Maybe Drink]
    -- ^ All drinks that need to be served. 'Nothing' indicates the drink was
    -- served or cancelled.
  , _tabStateOutstandingFood :: [Maybe Food]
    -- ^ All food that needs to be made. 'Nothing' indicates the food was made
    -- or cancelled.
  , _tabStatePreparedFood :: [Maybe Food]
    -- ^ All food that has been made. 'Nothing' indicates the food was served.
  , _tabStateServedItems :: [OrderedItem]
    -- ^ All items that have been served.
  } deriving (Show, Eq)

newtype Drink = Drink { unDrink :: OrderedItem }
  deriving (Show, Eq)

newtype Food = Food { unFood :: OrderedItem }
  deriving (Show, Eq)

data OrderedItem =
  OrderedItem
  { orderedItemMenuId :: Int
  , orderedItemDescription :: String
  , orderedItemPrice :: Double
  } deriving (Show, Eq)

makeLenses ''TabState

tabSeed :: TabState
tabSeed = TabState True [] [] [] []

data TabEvent
  = DrinksOrdered [Drink]
  | FoodOrdered [Food]
  | DrinksCancelled [Int]
  | FoodCancelled [Int]
  | DrinksServed [Int]
  | FoodPrepared [Int]
  | FoodServed [Int]
  | TabClosed Double
  deriving (Show, Eq)

tabApplyEvent :: TabState -> TabEvent -> TabState
tabApplyEvent state (DrinksOrdered drinks) = state & tabStateOutstandingDrinks %~ (++ map Just drinks)
tabApplyEvent state (FoodOrdered food) = state & tabStateOutstandingFood %~ (++ map Just food)
tabApplyEvent state (DrinksCancelled indexes) = state & tabStateOutstandingDrinks %~ setIndexesToNothing indexes
tabApplyEvent state (FoodCancelled indexes) = state & tabStateOutstandingFood %~ setIndexesToNothing indexes
tabApplyEvent state (DrinksServed indexes) =
  state
  & tabStateOutstandingDrinks %~ setIndexesToNothing indexes
  & tabStateServedItems %~
    \items -> items ++ fmap unDrink (catMaybes $ getListItemsByIndexes indexes (state ^. tabStateOutstandingDrinks))
tabApplyEvent state (FoodPrepared indexes) =
  state
  & tabStateOutstandingFood %~ setIndexesToNothing indexes
  & tabStatePreparedFood %~
    \items -> items ++ getListItemsByIndexes indexes (state ^. tabStatePreparedFood)
tabApplyEvent state (FoodServed indexes) =
  state
  & tabStatePreparedFood %~ setIndexesToNothing indexes
  & tabStateServedItems %~
    \items -> items ++ fmap unFood (catMaybes $ getListItemsByIndexes indexes (state ^. tabStatePreparedFood))
tabApplyEvent state (TabClosed _) = state & tabStateIsOpen .~ False

setIndexesToNothing :: [Int] -> [Maybe a] -> [Maybe a]
setIndexesToNothing indexes = map (\(i, x) -> if i `elem` indexes then Nothing else x) . zip [0..]

getListItemsByIndexes :: [Int] -> [a] -> [a]
getListItemsByIndexes indexes = map snd . filter ((`elem` indexes) . fst) . zip [0..]

type TabProjection = Projection TabState TabEvent

tabProjection :: TabProjection
tabProjection = Projection tabSeed tabApplyEvent

data TabCommand
  = PlaceOrder [Food] [Drink]
  | MarkDrinksServed [Int]
  | MarkFoodPrepared [Int]
  | MarkFoodServed [Int]
  | CloseTab Double
  deriving (Show, Eq)

data TabCommandError
  = TabAlreadyClosed
  | CannotCancelServedItem
  | TabHasUnservedItems
  | MustPayEnough
  deriving (Show, Eq)

tabApplyCommand :: TabState -> TabCommand -> Either TabCommandError [TabEvent]
tabApplyCommand state (CloseTab cash)
  | amountOfNonServedItems > 0 = Left TabHasUnservedItems
  | cash < totalServedWorth = Left MustPayEnough
  | otherwise = Right [TabClosed cash]
  where
    amountOfNonServedItems =
      length (state ^. tabStateOutstandingDrinks) +
      length (state ^. tabStateOutstandingFood) +
      length (state ^. tabStatePreparedFood)
    totalServedWorth = foldl' (+) 0 (fmap orderedItemPrice $ state ^. tabStateServedItems)
tabApplyCommand TabState { _tabStateIsOpen = False } _ = Left TabAlreadyClosed
tabApplyCommand _ (PlaceOrder food drinks) = Right [FoodOrdered food, DrinksOrdered drinks]
tabApplyCommand _ (MarkDrinksServed indexes) = Right [DrinksServed indexes]
tabApplyCommand _ (MarkFoodPrepared indexes) = Right [FoodPrepared indexes]
tabApplyCommand _ (MarkFoodServed indexes) = Right [FoodServed indexes]

type TabAggregate = Aggregate TabState TabEvent TabCommand TabCommandError

tabAggregate :: TabAggregate
tabAggregate = Aggregate tabApplyCommand
