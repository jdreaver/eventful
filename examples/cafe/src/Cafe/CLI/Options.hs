{-# LANGUAGE ScopedTypeVariables #-}

module Cafe.CLI.Options
  ( runOptionsParser
  , Options (..)
  , Command (..)
  , parseDatabaseFileOption
  ) where

import Data.Monoid ((<>))
import Database.Persist.Sql
import Options.Applicative
import Safe (atNote)

import Cafe.DB
import Cafe.Models.Tab

runOptionsParser :: IO Options
runOptionsParser = execParser $ info (helper <*> parseOptions) (fullDesc <> progDesc "Cafe CLI")

data Options
  = Options
  { optionsDatabaseFile :: FilePath
  , optionsCommand      :: Command
  } deriving (Show)

data Command
  = OpenTab
  | ListMenu
  | ViewTab TabEntityId
  | TabCommand TabEntityId TabCommand
  deriving (Show, Eq)

parseOptions :: Parser Options
parseOptions =
  Options <$>
  parseDatabaseFileOption <*>
  subparser (
    command "open-tab" (info (helper <*> pure OpenTab) (progDesc "Open a new tab")) <>
    command "list-menu" (info (helper <*> pure ListMenu) (progDesc "List all the menu items")) <>
    command "view-tab" (info (helper <*> (ViewTab <$> tabIdOption)) (progDesc "View current state of tab")) <>
    command "place-order" (info (helper <*> parseTabCommand parsePlaceOrder) (progDesc "Order food and drinks")) <>
    command "mark-drinks-served" (info (helper <*> parseTabCommand parseMarkDrinksServed) (progDesc "Mark drinks as served")) <>
    command "mark-food-prepared" (info (helper <*> parseTabCommand parseMarkFoodPrepared) (progDesc "Mark food as prepared")) <>
    command "mark-food-served" (info (helper <*> parseTabCommand parseMarkFoodServed) (progDesc "Mark food as served")) <>
    command "close-tab" (info (helper <*> parseTabCommand parseCloseTab) (progDesc "Close an open tab"))
  )

parseDatabaseFileOption :: Parser FilePath
parseDatabaseFileOption =
  strOption
    ( metavar "DATABASE_PATH" <>
      long "database-path" <>
      short 'p' <>
      value "database.db" <>
      help "File path for SQLite database. Default is ./database.db"
    )

tabIdOption :: Parser TabEntityId
tabIdOption =
  option (toSqlKey <$> auto)
  ( metavar "TAB_ID" <>
    short 'i' <>
    long "tab-id"
  )

parseTabCommand :: Parser TabCommand -> Parser Command
parseTabCommand commandParser = TabCommand <$> tabIdOption <*> commandParser

parsePlaceOrder :: Parser TabCommand
parsePlaceOrder =
  PlaceOrder <$>
  many
  ( option parseFood
    ( metavar "FOOD_INDEX" <>
      long "food"
    )
  ) <*>
  many
  ( option parseDrink
    ( metavar "DRINK_INDEX" <>
      long "drink"
    )
  )

parseMarkDrinksServed :: Parser TabCommand
parseMarkDrinksServed = MarkDrinksServed <$> repeatedIntOption "DRINK_INDEX" "drink-index"

parseMarkFoodPrepared :: Parser TabCommand
parseMarkFoodPrepared = MarkFoodPrepared <$> repeatedIntOption "FOOD_INDEX" "food-index"

parseMarkFoodServed :: Parser TabCommand
parseMarkFoodServed = MarkFoodServed <$> repeatedIntOption "FOOD_INDEX" "food-index"

parseCloseTab :: Parser TabCommand
parseCloseTab =
  CloseTab <$>
  option auto
  ( metavar "CASH" <>
    long "cash"
  )

-- | Parses a drink using an integer index into the drink list
parseDrink :: ReadM Drink
parseDrink = do
  (index :: Int) <- auto
  return $ atNote ("No drink exists for index " ++ show index) allDrinks index

-- | Parses a food using an integer index into the food list
parseFood :: ReadM Food
parseFood = do
  (index :: Int) <- auto
  return $ atNote ("No food exists for index " ++ show index) allFood index

repeatedIntOption :: String -> String -> Parser [Int]
repeatedIntOption meta' long' =
  many $ option auto $ metavar meta' <> long long'

-- parseUUID :: ReadM UUID
-- parseUUID = maybe (readerError "Could not parse UUID") return . uuidFromText =<< (toS <$> str)
