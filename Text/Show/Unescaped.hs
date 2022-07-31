{-|
Module      : Text.Show.Unescaped
Description : Convenience functions for changing how @show@ escapes characters
Copyright   : (c) Gregory Cox
License     : BSD-3
Maintainer  : gcox_gcox@proton.me
Stability   : experimental
Portability : Portable

This library provides a convenient way of configuring GHCi to print non-ASCII
output, and, similarly, a variation on the @'Text.Show.show'@ function that
does not escape non-ASCII characters.

For example, you can replace @'System.IO.print'@ with @'printUnescapePrintable'@,
which only escapes unprintable characters.
See [Configuring GHCi's print function](#g:1) below for instructions on how
to set this as the print function GHCi uses.

@
>>> printUnescapePrintable (Just \'÷\', \'±\')
(Just \'÷\', \'±\')
@

-}

module Text.Show.Unescaped (
      -- * Configuring GHCi's print function
      -- $printFunctions1
      -- $printFunctions2
      -- $printFunctions3
      -- $printFunctions4
      -- $printFunctions5
      printUnescapePrintable
    , printWithHex
    , printUnescapePrintableWithHex

      -- * Show functions
      -- $showFunctions1
      -- $showFunctions2
      -- $showFunctions3
      -- $showFunctions4
      -- $showFunctions5
      -- $showFunctions6
    , showUnescapePrintable
    , showWithHex
    , showUnescapePrintableWithHex

      -- * Conversion functions
      -- $conversionFunctions
    , unescapePrintable
    , withHex
    , unescapePrintableWithHex

    ) where

import Data.Char (ord, chr, isPrint, isDigit, isHexDigit)
import Data.List (foldr1, stripPrefix)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Maybe (listToMaybe, fromMaybe)
import Numeric (readDec, showHex)
import System.IO (putStrLn)

-- $printFunctions1
-- Functions whose name starts with @print@ are intended for use with the
-- @interactive-print@ option of GHCi,
-- [as explained in the GHC User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#using-a-custom-interactive-printing-function).
-- For example, you can use the following command-line option to set the print function:

-- $printFunctions2
-- @
-- ghci -interactive-print=printUnescapePrintableWithHex Text.Show.Unescaped
-- @

-- $printFunctions3
-- You can also set this option by adding the following line to your [@.ghci@ configuration file](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#the-ghci-files):

-- $printFunctions4
-- @
-- import Text.Show.Unescaped
-- :set -interactive-print=printUnescapePrintableWithHex
-- @
-- $printFunctions5
-- There are the following printing functions:

-- $showFunctions1
-- Functions whose name starts with @show@ are replacements for @'Text.Show.show'@.
-- You can use them directly, or you can choose one to replace @'Text.Show.show'@ with,
-- for example by adding the following to the import section of a source file:

-- $showFunctions2
-- @
-- import Text.Show.Unescaped (showUnescapePrintableWithHex)
-- import Prelude hiding (show)
-- import qualified Prelude as P (show)
-- @

-- $showFunctions3
-- and then add the following line to the body of the file:

-- $showFunctions4
-- @
-- show = showUnescapePrintableWithHex
-- @

-- $showFunctions5
-- This replaces @'Text.Show.show'@ but keeps the original @'Text.Show.show'@ function
-- available, renamed as @P.show@. To derive or implement the @'Text.Show.Show'@ class
-- for your own objects, you will need to use @P.show@.

-- $showFunctions6
-- There are the following three show functions:

-- $conversionFunctions
-- These are the basic functions used for parsing. They are less likely to be useful,
-- but if a function that uses @'Text.Show.show'@ internally has unwanted escape
-- sequences output by @'Text.Show.show'@, you can use these functions to remove or
-- replace those escape sequences.


data ParseResult
    = PassThrough
    | AsChar Char String
    | AsCode String String
    deriving (Eq, Show)

type EscapingPolicy = Char -> String -> ParseResult

-- | Like @'System.IO.print'@, but display printable characters unchanged
--   instead of escaping them.
-- 
-- >>> print ['\x9d'..'\xa5']
-- "\157\158\159\160\161\162\163\164\165"
-- >>> printUnescapePrintable ['\x9d'..'\xa5']
-- "\157\158\159 ¡¢£¤¥"
-- 
printUnescapePrintable :: Show a => a -> IO ()
printUnescapePrintable = putStrLn . showUnescapePrintable

-- | Like @'Text.Show.show'@, but display printable characters unchanged
--   instead of escaping them.
-- 
-- >>> putStrLn $ show ['\x9d'..'\xa5']
-- "\157\158\159\160\161\162\163\164\165"
-- >>> putStrLn $ show UnescapePrintable ['\x9d'..'\xa5']
-- "\157\158\159 ¡¢£¤¥"
-- 
showUnescapePrintable :: Show a => a -> String
showUnescapePrintable = unescapePrintable . show

-- | Convert the output of @'Text.Show.show'@ to display printable
--   characters unchanged instead of escaping them.
-- 
-- >>> putStrLn $ show ['\x9d'..'\xa5']
-- "\157\158\159\160\161\162\163\164\165"
-- >>> putStrLn $ unescapePrintable $ show ['\x9d'..'\xa5']
-- "\157\158\159 ¡¢£¤¥"
-- 
unescapePrintable :: String -> String
unescapePrintable = unescapeUnicodeGeneral decPrintable

decPrintable :: EscapingPolicy
decPrintable c s =
    if isPrint c then AsChar c s else PassThrough

-- | Like @'System.IO.print'@, but use hexadecimal instead of decimal
--   in escape codes.
-- 
-- >>> print ['\x9d'..'\xa5']
-- "\157\158\159\160\161\162\163\164\165"
-- >>> printWithHex ['\x9d'..'\xa5']
-- "\x9d\x9e\x9f\xa0\xa1\xa2\xa3\xa4\xa5"
-- 
printWithHex :: Show a => a -> IO ()
printWithHex = putStrLn . showWithHex

-- | Like @'Text.Show.show'@, but use hexadecimal instead of decimal
--   in escape codes.
-- 
-- >>> putStrLn $ show ['\x9d'..'\xa5']
-- "\157\158\159\160\161\162\163\164\165"
-- >>> putStrLn $ showWithHex ['\x9d'..'\xa5']
-- "\x9d\x9e\x9f\xa0\xa1\xa2\xa3\xa4\xa5"
-- 
showWithHex :: Show a => a -> String
showWithHex = withHex . show

-- | Convert the output of @'Text.Show.show'@ to use hexadecimal
--   instead of decimal in escape codes.
-- 
-- >>> putStrLn $ show ['\x9d'..'\xa5']
-- "\157\158\159\160\161\162\163\164\165"
-- >>> putStrLn $ withHex $ show ['\x9d'..'\xa5']
-- "\x9d\x9e\x9f\xa0\xa1\xa2\xa3\xa4\xa5"
-- 
withHex :: String -> String
withHex = unescapeUnicodeGeneral decToHex

decToHex :: EscapingPolicy
decToHex c s = AsCode (toHexCode c s) s

-- | Like @'System.IO.print'@, but display printable characters unchanged
--   instead of escaping them, and use hexadecimal instead of decimal
--   in escape codes.
-- 
-- >>> print ['\x9d'..'\xa5']
-- "\157\158\159\160\161\162\163\164\165"
-- >>> printUnescapePrintableWithHex ['\x9d'..'\xa5']
-- "\x9d\x9e\x9f ¡¢£¤¥"
-- 
printUnescapePrintableWithHex :: Show a => a -> IO ()
printUnescapePrintableWithHex = putStrLn . showUnescapePrintableWithHex

-- | Like @'Text.Show.show'@, but display printable characters unchanged
--   instead of escaping them, and use hexadecimal instead of decimal
--   in escape codes.
-- 
-- >>> putStrLn $ show ['\x9d'..'\xa5']
-- "\157\158\159\160\161\162\163\164\165"
-- >>> putStrLn $ show UnescapePrintableWithHex ['\x9d'..'\xa5']
-- "\x9d\x9e\x9f ¡¢£¤¥"
-- 
showUnescapePrintableWithHex :: Show a => a -> String
showUnescapePrintableWithHex = unescapePrintableWithHex . show

-- | Convert the output of @'Text.Show.show'@ to display printable
--   characters unchanged instead of escaping them, and use
--   hexadecimal instead of decimal in escape codes.
-- 
-- >>> putStrLn $ show ['\x9d'..'\xa5']
-- "\157\158\159\160\161\162\163\164\165"
-- >>> putStrLn $ unescapePrintableWithHex $ show ['\x9d'..'\xa5']
-- "\x9d\x9e\x9f ¡¢£¤¥"
-- 
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
                      -- "\&" marks the end of an escape sequence in ambiguous cases
                      -- that occur when the escape is followed by a literal digit.
                      -- For example, this differentiates ['\1000','9'] (shown as "\1000\&9")
                      -- from ['\10009'] (shown as "\10009").
                      -- When the escape sequence is replaced by a literal, it
                      -- becomes unnecessary, and can be deleted.
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
