module Unescaped
    ( printUnescaped
    , showUnescaped
    , unescapeUnicode
    ) where

import Data.Char (ord, chr, isPrint, isDigit)
import Data.List (foldl')
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
          escapeCode cs@(c:s)
              | isDigit c = splitInt cs
              | otherwise = '\\' : c : unescapeUnicode s
          splitInt s =
              let (ds, s') = span isDigit s
                  in case (testIntStr ds, s') of
                         (Nothing, _) -> ('\\' : ds) ++ unescapeUnicode s'
                         (Just c, '\\' : '&' : suffix) ->
                                        c : unescapeUnicode suffix
                         (Just c, _) -> c : unescapeUnicode s'
          testIntStr :: String -> Maybe Char
          testIntStr s
              | isPrint ch = Just ch
              | otherwise = Nothing
              where ch = chr . readDec . map fromDigit $ s
                        where fromDigit c = ord c - ord '0'
                              readDec :: [Int] -> Int
                              readDec = foldl' step 0
                                  where step a b = a * 10 + b
