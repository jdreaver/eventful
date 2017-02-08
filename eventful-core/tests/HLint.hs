module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

import Prelude (String, IO, null)

arguments :: [String]
arguments =
    [ "src"
    , "tests"
    , "-i=Redundant do"
    , "-i=Unused LANGUAGE pragma" -- This fails on DeriveGeneric
    , "-i=Use newtype instead of data"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure
