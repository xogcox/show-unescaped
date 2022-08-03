module Main (main) where

import Text.Show.Unescaped

import Test.QuickCheck
import Control.Monad (unless)

import System.Exit (exitFailure)

main :: IO ()
main = (sequence $ check <$> [idempotent, readShow] <*> fs)
         >>= exitOnFailure
    where fs = 
             [ unescapePrintable
             , withHex
             , unescapePrintableWithHex
             ]

exitOnFailure :: [Result] -> IO ()
exitOnFailure x = unless (all isSuccess x) exitFailure

check :: ((String -> String) -> String -> Bool) -> (String -> String) -> IO Result
check t f = quickCheckResult (t f)

idempotent :: (String -> String) -> String -> Bool
idempotent f x = f x == f (f x)

readShow :: (String -> String) -> String -> Bool
readShow f x = (read . f . show $ x) == x
