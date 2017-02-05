module Cafe.CLI.Options
  ( runOptionsParser
  , Options (..)
  , Command (..)
  ) where

import Data.Monoid ((<>))
import Database.Persist.Sql
import Options.Applicative

import Cafe.DB
import Cafe.Models.Tab

runOptionsParser :: IO Options
runOptionsParser = execParser $ info (helper <*> parseOptions) (fullDesc <> progDesc "Cafe CLI")

data Options
  = Options
  { optionsDatabaseFile :: FilePath
  , optionsCommand :: Command
  } deriving (Show)

data Command
  = OpenTab
  | TabCommand TabEntityId TabCommand
  deriving (Show, Eq)

parseOptions :: Parser Options
parseOptions =
  Options <$>
  strOption
    ( metavar "DATABASE_PATH" <>
      long "database-path" <>
      short 'p' <>
      value "database.db" <>
      help "File path for SQLite database. Default is ./database.db"
    ) <*>
  subparser (
    command "open-tab" (info (helper <*> pure OpenTab) (progDesc "Open a new tab")) <>
    command "close-tab" (info (helper <*> parseTabCommand parseCloseTab) (progDesc "Close an open tab"))
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

parseCloseTab :: Parser TabCommand
parseCloseTab =
  CloseTab <$>
  option auto
  ( metavar "CASH" <>
    long "cash"
  )

-- parseUUID :: ReadM UUID
-- parseUUID = maybe (readerError "Could not parse UUID") return . uuidFromText =<< (toS <$> str)
