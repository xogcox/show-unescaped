# Changing how Haskell `show` and `print` escape Unicode characters

The `show` and `print` functions (and the outputs in `GHCi`) replace
all non-ASCII characters[^1] with escape codes. For example:

```haskell
ghci> '÷'
'\247'
```

This is useful in some cases: perhaps you are printing user-entered
text into a logfile, and want to make sure it is free of aberrations
like [Zalgo text](https://en.wikipedia.org/wiki/Zalgo_text).
But what if you have strings or objects containing accented characters,
symbols, emoji or non-Latin scripts that you want to print verbatim?

This module provides convenience functions for replacing `show` and `print`
so that these characters are not escaped.

## Comments?

Please feel free to submit issues or pull requests with criticism,
alternative proposals, suggestions for name changes or any other comments.

## Choosing which function to use

You have three choices to make:

1) Which characters should be escaped?

 * All non-ASCII characters (the same as `show` and `print`)
 * Only unprintable characters

2) What format should be used for escape codes?

 * Decimal (like `"\247"`)
 * Hexadecimal (like `"\xf7"`)

3) Which function are you replacing?

 * `print`: The function used to output results in `GHCi`
 * `show`: The function that converts objects from a variety of types to the `String` type
 * Other functions: If there is a function that behaves like `show`, or
is implemented using`show`, you can change its escape characters by
applying one of the functions listed as "conversion functions" to its output.

Depending on the choices above, select the function to use from the following table:

| Escape which? | All non-ASCII | All non-ASCII | Unprintable only | Unprintable only |
Escape code format? | Decimal | Hexadecimal | Decimal | Hexadecimal |
| --- | --- | --- | --- | --- |
Replacement for `print` | `print` | `printWithHex` | `printUnescapePrintable` | `printUnescapePrintableWithHex` |
Replacement for `show` | `show` | `showWithHex` | `showUnescapePrintable` | `showUnescapePrintableWithHex` |
Conversion functions | `id` | `withHex` | `unescapePrintable` | `unescapePrintablewithHex` |

Of course, the functions in the leftmost column are the standard functions, and `id` indicates that
no conversion is necessary because `show` already does what you want.

## Implementation

The conversion functions are functions of type `String -> String` that simply
parse their input to find escape codes and replace them if necessary.
(Escape codes for ASCII characters, such as `'\NUL'` and `'\n'`, are left unchanged.)

The other functions are defined using the conversion functions, for example:

```haskell
showUnescapePrintable = unescapePrintable . show

printUnescapePrintable = putStrLn . unescapePrintable . show
```

## Compatibility with `Read`

The output of all the functions should be compatible with `read`. That is,
in the case of `unescapePrintable`, for example, for all `a`:

```haskell
read (showUnescapePrintable a) == read (show a)
read (unescapePrintable a) == read a
```

## Replacing `print`

Functions whose name starts with `print` are intended for use with the
`interactive-print` option of `GHCi`, [as explained in the GHC User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#using-a-custom-interactive-printing-function).
For example, you can use the following command-line option to set the print function:

```
ghci -interactive-print=printUnescapePrintableWithHex Text.Show.Unescaped
```

You can also set this option by adding the following line to your [`.ghci` configuration file](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#the-ghci-files):

```
import Text.Show.Unescaped
:set -interactive-print=printUnescapePrintableWithHex
```

## Replacing `show`

Functions whose name starts with `show` are replacements for the `show` function.
You can use them directly, or you can choose one to replace `show` with,
for example by adding the following to the import section of a source file:

```haskell
import Text.Show.Unescaped (showUnescapePrintableWithHex)
import Prelude hiding (show)
import qualified Prelude as P (show)
```

and then add the following line to the body of the file:

```haskell
show = showUnescapePrintableWithHex
```

This replaces `show` but keeps the original `show` function
available, renamed as `P.show`. To derive or implement the `Show` class
for your own objects, you will need to use `P.show`.

### Note on terminology: What are ASCII and non-ASCII characters?

[^1]: In this explanation, "ASCII characters" refer to what the official
Unicode terminology calls the Unicode Basic Latin Block, which
consists of the first 128 Unicode characters (officially called
"code points"), since these characters are identical to the ASCII
character set. "Non-ASCII characters" means all characters outside
this block.
