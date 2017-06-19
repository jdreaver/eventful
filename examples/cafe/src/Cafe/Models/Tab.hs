{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , setIndexesToNothing
  ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.List (foldl')
import Data.Maybe (catMaybes, isJust)

import Eventful

data MenuItem =
  MenuItem
  { menuItemDescription :: String
  , menuItemPrice :: Double
  } deriving (Show, Eq)

deriveJSON (aesonPrefix camelCase) ''MenuItem

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
deriveJSON (aesonPrefix camelCase) ''TabState

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

deriveJSON (aesonPrefix camelCase) ''TabEvent

applyTabEvent :: TabState -> TabEvent -> TabState
applyTabEvent state (DrinksOrdered drinks) = state & tabStateOutstandingDrinks %~ (++ map Just drinks)
applyTabEvent state (FoodOrdered food) = state & tabStateOutstandingFood %~ (++ map Just food)
applyTabEvent state (DrinksCancelled indexes) = state & tabStateOutstandingDrinks %~ setIndexesToNothing indexes
applyTabEvent state (FoodCancelled indexes) = state & tabStateOutstandingFood %~ setIndexesToNothing indexes
applyTabEvent state (DrinksServed indexes) =
  state
  & tabStateServedItems %~
    (\items -> items ++ fmap unDrink (catMaybes $ getListItemsByIndexes indexes (state ^. tabStateOutstandingDrinks)))
  & tabStateOutstandingDrinks %~ setIndexesToNothing indexes
applyTabEvent state (FoodPrepared indexes) =
  state
  & tabStatePreparedFood %~
    (\items -> items ++ getListItemsByIndexes indexes (state ^. tabStateOutstandingFood))
  & tabStateOutstandingFood %~ setIndexesToNothing indexes
applyTabEvent state (FoodServed indexes) =
  state
  & tabStateServedItems %~
    (\items -> items ++ fmap unFood (catMaybes $ getListItemsByIndexes indexes (state ^. tabStatePreparedFood)))
  & tabStatePreparedFood %~ setIndexesToNothing indexes
applyTabEvent state (TabClosed _) = state & tabStateIsOpen .~ False

setIndexesToNothing :: [Int] -> [Maybe a] -> [Maybe a]
setIndexesToNothing indexes = map (\(i, x) -> if i `elem` indexes then Nothing else x) . zip [0..]

getListItemsByIndexes :: [Int] -> [a] -> [a]
getListItemsByIndexes indexes = map snd . filter ((`elem` indexes) . fst) . zip [0..]

type TabProjection = Projection TabState TabEvent

tabProjection :: TabProjection
tabProjection = Projection tabSeed applyTabEvent

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

applyTabCommand :: TabState -> TabCommand -> Either TabCommandError [TabEvent]
applyTabCommand TabState { _tabStateIsOpen = False } _ = Left TabAlreadyClosed
applyTabCommand state (CloseTab cash)
  | amountOfNonServedItems > 0 = Left TabHasUnservedItems
  | cash < totalServedWorth = Left MustPayEnough
  | otherwise = Right [TabClosed cash]
  where
    amountOfNonServedItems =
      length (filter isJust $ state ^. tabStateOutstandingDrinks) +
      length (filter isJust $ state ^. tabStateOutstandingFood) +
      length (filter isJust $ state ^. tabStatePreparedFood)
    totalServedWorth = foldl' (+) 0 (fmap menuItemPrice $ state ^. tabStateServedItems)
applyTabCommand _ (PlaceOrder food drinks) = Right [FoodOrdered food, DrinksOrdered drinks]
-- TODO: Check if index exceeds list length or if item is already marked null
-- for the next 3 commands.
applyTabCommand _ (MarkDrinksServed indexes) = Right [DrinksServed indexes]
applyTabCommand _ (MarkFoodPrepared indexes) = Right [FoodPrepared indexes]
applyTabCommand _ (MarkFoodServed indexes) = Right [FoodServed indexes]

type TabAggregate = Aggregate TabState TabEvent TabCommand

tabAggregate :: TabAggregate
tabAggregate = Aggregate (either (const []) id `compose` applyTabCommand) tabProjection
  where compose = (.).(.)

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
