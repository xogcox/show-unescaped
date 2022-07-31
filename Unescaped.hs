module Unescaped
    ( printUnescaped
    , showUnescaped
    , unescapeUnicode
    ) where

import Data.Char (ord, chr, isPrint, isDigit)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Maybe (listToMaybe)
import Numeric (readDec)
import System.IO (putStrLn)

printUnescaped :: Show a => a -> IO ()
printUnescaped = putStrLn . showUnescaped

showUnescaped :: Show a => a -> String
showUnescaped = unescapeUnicode . show

unescapeUnicode :: String -> String
unescapeUnicode "" = ""
unescapeUnicode (c:s) =
    case c of
        '\\' -> escapeCode s
        _ -> c : unescapeUnicode s
    where escapeCode [] = "\\" -- A final backslash is an illegal escape sequence.
                               -- However, pass it through instead of doing validation.
          escapeCode cs@(c:s) =
              case parseDec cs of
                  Nothing       -> '\\' : c : unescapeUnicode s
                  Just (c', '\\' : '&' : suffix)
                                -> c' : unescapeUnicode suffix
                  Just (c', s') -> c' : unescapeUnicode s'
          parseDec :: String -> Maybe (Char, String)
          parseDec s
              | tooManyDigits s = Nothing
              | otherwise = listToMaybe (readDec s) >>= verifyCharInRange
              where maxOrd :: Int
                    maxOrd = ord (maxBound :: Char)
                    tooManyDigits :: String -> Bool
                    tooManyDigits = all isDigit . take (maxOrdDigits + 1)
                        where maxOrdDigits = length . takeWhile (>0) .
                                  iterate (`div` 10) $ maxOrd
                    verifyCharInRange :: (Int, String) -> Maybe (Char, String)
                    verifyCharInRange (i, s)
                        | i > maxOrd        = Nothing
                        | not . isPrint $ c = Nothing
                        | otherwise         = Just (c, s)
                        where c = chr i

