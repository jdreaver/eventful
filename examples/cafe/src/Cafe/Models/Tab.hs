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

import Eventful

data TabState =
  TabState
  { _tabStateIsOpen :: Bool
  , _tabStateOutstandingDrinks :: [Drink]
  , _tabStateOutstandingFood :: [Food]
  , _tabStatePreparedFood :: [Food]
  , _tabStateServedItems :: [OrderedItem]
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
tabApplyEvent state (DrinksOrdered drinks) = state & tabStateOutstandingDrinks %~ (++ drinks)
tabApplyEvent state (FoodOrdered food) = state & tabStateOutstandingFood %~ (++ food)
tabApplyEvent state (DrinksCancelled indexes) = state & tabStateOutstandingDrinks %~ filterListByIndexes indexes
tabApplyEvent state (FoodCancelled indexes) = state & tabStateOutstandingFood %~ filterListByIndexes indexes
tabApplyEvent state (DrinksServed indexes) =
  state
  & tabStateOutstandingDrinks %~ filterListByIndexes indexes
  & tabStateServedItems %~
    \items -> items ++ fmap unDrink (getListItemsByIndexes indexes (state ^. tabStateOutstandingDrinks))
tabApplyEvent state (FoodPrepared indexes) =
  state
  & tabStateOutstandingFood %~ filterListByIndexes indexes
  & tabStatePreparedFood %~
    \items -> items ++ getListItemsByIndexes indexes (state ^. tabStatePreparedFood)
tabApplyEvent state (FoodServed indexes) =
  state
  & tabStatePreparedFood %~ filterListByIndexes indexes
  & tabStateServedItems %~
    \items -> items ++ fmap unFood (getListItemsByIndexes indexes (state ^. tabStatePreparedFood))
tabApplyEvent state (TabClosed _) = state & tabStateIsOpen .~ False

filterListByIndexes :: [Int] -> [a] -> [a]
filterListByIndexes indexes = map snd . filter (not . (`elem` indexes) . fst) . zip [0..]

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
  = CannotCancelServedItem
  | TabHasUnservedItems
  | MustPayEnough
  deriving (Show, Eq)

tabApplyCommand :: TabState -> TabCommand -> Either TabCommandError [TabEvent]
tabApplyCommand _ (PlaceOrder food drinks) = Right [FoodOrdered food, DrinksOrdered drinks]
tabApplyCommand _ (MarkDrinksServed indexes) = Right [DrinksServed indexes]
tabApplyCommand _ (MarkFoodPrepared indexes) = Right [FoodPrepared indexes]
tabApplyCommand _ (MarkFoodServed indexes) = Right [FoodServed indexes]
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

type TabAggregate = Aggregate TabState TabEvent TabCommand TabCommandError

tabAggregate :: TabAggregate
tabAggregate = Aggregate tabApplyCommand
