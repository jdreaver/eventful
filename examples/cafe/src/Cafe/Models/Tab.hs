module Cafe.Models.Tab
  ( TabState (..)
  , Drink (..)
  , Food (..)
  , MenuItem (..)
  , allDrinks
  , allFood
  , TabProjection
  , tabProjection
  , TabAggregate
  , tabAggregate
  , TabCommand (..)
  , TabEvent (..)
  , TabCommandError (..)
  ) where

import Control.Lens
import Data.List (foldl')
import Data.Maybe (catMaybes, isJust)

import Eventful

data MenuItem =
  MenuItem
  { menuItemDescription :: String
  , menuItemPrice :: Double
  } deriving (Show, Eq)

deriveJSON (unPrefix "menuItem") ''MenuItem

newtype Drink = Drink { unDrink :: MenuItem }
  deriving (Show, Eq, FromJSON, ToJSON)

newtype Food = Food { unFood :: MenuItem }
  deriving (Show, Eq, FromJSON, ToJSON)

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
  , _tabStateServedItems :: [MenuItem]
    -- ^ All items that have been served.
  } deriving (Show, Eq)

makeLenses ''TabState
deriveJSON (unPrefix "_tabState") ''TabState

tabSeed :: TabState
tabSeed = TabState True [] [] [] []

data TabEvent
  = DrinksOrdered
    { _tabEventDrinks :: [Drink]
    }
  | FoodOrdered
    { _tabEventFood :: [Food]
    }
  | DrinksCancelled
    { _tabEventDrinkIndexes :: [Int]
    }
  | FoodCancelled
    { _tabEventFoodIndexes :: [Int]
    }
  | DrinksServed
    { _tabEventDrinkIndexes :: [Int]
    }
  | FoodPrepared
    { _tabEventFoodIndexes :: [Int]
    }
  | FoodServed
    { _tabEventFoodIndexes :: [Int]
    }
  | TabClosed Double
  deriving (Show, Eq)

deriveJSON (unPrefix "_tabEvent") ''TabEvent

tabApplyEvent :: TabState -> TabEvent -> TabState
tabApplyEvent state (DrinksOrdered drinks) = state & tabStateOutstandingDrinks %~ (++ map Just drinks)
tabApplyEvent state (FoodOrdered food) = state & tabStateOutstandingFood %~ (++ map Just food)
tabApplyEvent state (DrinksCancelled indexes) = state & tabStateOutstandingDrinks %~ setIndexesToNothing indexes
tabApplyEvent state (FoodCancelled indexes) = state & tabStateOutstandingFood %~ setIndexesToNothing indexes
tabApplyEvent state (DrinksServed indexes) =
  state
  & tabStateServedItems %~
    (\items -> items ++ fmap unDrink (catMaybes $ getListItemsByIndexes indexes (state ^. tabStateOutstandingDrinks)))
  & tabStateOutstandingDrinks %~ setIndexesToNothing indexes
tabApplyEvent state (FoodPrepared indexes) =
  state
  & tabStatePreparedFood %~
    (\items -> items ++ getListItemsByIndexes indexes (state ^. tabStateOutstandingFood))
  & tabStateOutstandingFood %~ setIndexesToNothing indexes
tabApplyEvent state (FoodServed indexes) =
  state
  & tabStateServedItems %~
    (\items -> items ++ fmap unFood (catMaybes $ getListItemsByIndexes indexes (state ^. tabStatePreparedFood)))
  & tabStatePreparedFood %~ setIndexesToNothing indexes
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
  -- CancelDrinks [Int]
  -- CancelFood [Int]
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
tabApplyCommand TabState { _tabStateIsOpen = False } _ = Left TabAlreadyClosed
tabApplyCommand state (CloseTab cash)
  | amountOfNonServedItems > 0 = Left TabHasUnservedItems
  | cash < totalServedWorth = Left MustPayEnough
  | otherwise = Right [TabClosed cash]
  where
    amountOfNonServedItems =
      length (filter isJust $ state ^. tabStateOutstandingDrinks) +
      length (filter isJust $ state ^. tabStateOutstandingFood) +
      length (filter isJust $ state ^. tabStatePreparedFood)
    totalServedWorth = foldl' (+) 0 (fmap menuItemPrice $ state ^. tabStateServedItems)
tabApplyCommand _ (PlaceOrder food drinks) = Right [FoodOrdered food, DrinksOrdered drinks]
-- TODO: Check if index exceeds list length or if item is already marked null
-- for the next 3 commands.
tabApplyCommand _ (MarkDrinksServed indexes) = Right [DrinksServed indexes]
tabApplyCommand _ (MarkFoodPrepared indexes) = Right [FoodPrepared indexes]
tabApplyCommand _ (MarkFoodServed indexes) = Right [FoodServed indexes]

type TabAggregate = Aggregate TabState TabEvent TabCommand TabCommandError

tabAggregate :: TabAggregate
tabAggregate = Aggregate tabApplyCommand tabProjection

-- | List of all drinks. The menu could be its own aggregate in the future.
allDrinks :: [Drink]
allDrinks =
  map Drink
  [ MenuItem "Beer" 3.50
  , MenuItem "Water" 0.00
  , MenuItem "Soda" 1.00
  ]

-- | List of all food. The menu could be its own aggregate in the future.
allFood :: [Food]
allFood =
  map Food
  [ MenuItem "Sandwich" 6.50
  , MenuItem "Steak" 10.00
  , MenuItem "Chips" 1.00
  ]
