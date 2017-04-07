-- | Utilities for JSON serialization

module Bank.Json
  ( unPrefixLower
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Monoid ((<>))

-- | Aeson Options that match FrontRow usage
unPrefixLower :: String -> Options
unPrefixLower prefix = defaultOptions
  { fieldLabelModifier = unCapitalize . dropPrefix prefix }

unCapitalize :: String -> String
unCapitalize [] = []
unCapitalize (c:cs) = toLower c : cs

dropPrefix :: String -> String -> String
dropPrefix prefix input = go prefix input
  where
    go pre [] = error $ contextual $ "prefix leftover: " <> pre
    go [] (c:cs) = c : cs
    go (p:preRest) (c:cRest)
      | p == c = go preRest cRest
      | otherwise = error $ contextual $ "not equal: " <>  (p:preRest)  <> " " <> (c:cRest)

    contextual msg = "dropPrefix: " <> msg <> ". " <> prefix <> " " <> input
