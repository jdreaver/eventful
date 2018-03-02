-- | Utilities for JSON serialization

module Bank.Json
  ( unPrefixLower
  , dropPrefix
  , dropSuffix
  , deriveJSONUnPrefixLower
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)
import Language.Haskell.TH

-- | Aeson Options that match FrontRow usage
unPrefixLower :: String -> Options
unPrefixLower prefix = defaultOptions
  { fieldLabelModifier = unCapitalize . dropPrefix prefix }

unCapitalize :: String -> String
unCapitalize [] = []
unCapitalize (c:cs) = toLower c : cs

dropPrefix :: String -> String -> String
dropPrefix = dropPrefix' "dropPrefix" id

dropSuffix :: String -> String -> String
dropSuffix prefix input = reverse $ dropPrefix' "dropSuffix" reverse (reverse prefix) (reverse input)

dropPrefix' :: String -> (String -> String) -> String -> String -> String
dropPrefix' fnName strTrans prefix input = go prefix input
  where
    go pre [] = error $ contextual $ "prefix leftover: " ++ strTrans pre
    go [] (c:cs) = c : cs
    go (p:preRest) (c:cRest)
      | p == c = go preRest cRest
      | otherwise = error $ contextual $ "not equal: " ++  strTrans (p:preRest)  ++ " " ++ strTrans (c:cRest)
    contextual msg = fnName ++ ": " ++ msg ++ ". " ++ strTrans prefix ++ " " ++ strTrans input

deriveJSONUnPrefixLower :: Name -> Q [Dec]
deriveJSONUnPrefixLower name = deriveJSON (unPrefixLower $ firstCharToLower $ nameBase name) name

firstCharToLower :: String -> String
firstCharToLower [] = []
firstCharToLower (x:xs) = toLower x : xs
