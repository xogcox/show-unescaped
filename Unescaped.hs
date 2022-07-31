module Unescaped
    ( printUnescapePrintable
    , showUnescapePrintable
    , unescapePrintable
    , printWithHex
    , showWithHex
    , withHex
    , printUnescapePrintableWithHex
    , showUnescapePrintableWithHex
    , unescapePrintableWithHex
    ) where

import Data.Char (ord, chr, isPrint, isDigit, isHexDigit)
import Data.List (foldr1, stripPrefix)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Maybe (listToMaybe, fromMaybe)
import Numeric (readDec, showHex)
import System.IO (putStrLn)

data ParseResult
    = PassThrough
    | AsChar Char String
    | AsCode String String
    deriving (Eq, Show)

type EscapingPolicy = Char -> String -> ParseResult

printUnescapePrintable :: Show a => a -> IO ()
printUnescapePrintable = putStrLn . showUnescapePrintable

showUnescapePrintable :: Show a => a -> String
showUnescapePrintable = unescapePrintable . show

unescapePrintable :: String -> String
unescapePrintable = unescapeUnicodeGeneral decPrintable

decPrintable :: EscapingPolicy
decPrintable c s =
    if isPrint c then AsChar c s else PassThrough

printWithHex :: Show a => a -> IO ()
printWithHex = putStrLn . showWithHex

showWithHex :: Show a => a -> String
showWithHex = withHex . show

withHex :: String -> String
withHex = unescapeUnicodeGeneral decToHex

decToHex :: EscapingPolicy
decToHex c s = AsCode (toHexCode c s) s

printUnescapePrintableWithHex :: Show a => a -> IO ()
printUnescapePrintableWithHex = putStrLn . showUnescapePrintableWithHex

showUnescapePrintableWithHex :: Show a => a -> String
showUnescapePrintableWithHex = unescapePrintableWithHex . show

unescapePrintableWithHex :: String -> String
unescapePrintableWithHex = unescapeUnicodeGeneral hexPrintable

hexPrintable :: EscapingPolicy
hexPrintable c s =
    if isPrint c
        then AsChar c s
        else AsCode (toHexCode c s) s

unescapeUnicodeGeneral :: EscapingPolicy -> String -> String
unescapeUnicodeGeneral convert s =
    unfoldString go s
    where go (c :| s) =
              case c of
                  '\\' -> escapeCode s
                  _ -> ([c], s)
          escapeCode [] = ("\\", "") -- A final backslash is an illegal escape sequence.
                                     -- However, pass it through instead of doing validation.
          escapeCode cs@(c:s) =
              case selectOutput (parseDec cs) of
                  PassThrough  -> (['\\', c], s)
                  AsChar c' s' -> ([c'], removeEscape s')
                  AsCode x s   -> (x, s)
              where removeEscape s = fromMaybe s . stripPrefix "\\&" $ s
                    selectOutput p = case p of
                        Nothing -> PassThrough
                        Just (c, s) -> convert c s

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
              | otherwise         = Just (c, s)
              where c = chr i

toHexCode :: Char -> String -> String
toHexCode c s =
    if needsEscape then hx ++ "\\&" else hx
    where needsEscape = maybe False isHexDigit . listToMaybe $ s
          hx = "\\x" ++ showHex (ord c) ""

unfoldString :: (NonEmpty Char -> (String, String)) -> String -> String
unfoldString f s =
    concat . takeWhile (not . null) . map fst . tail . iterate go $ ("", s)
    where go :: (String, String) -> (String, String)
          go (_, s) = maybe ("", "") f . nonEmpty $ s
