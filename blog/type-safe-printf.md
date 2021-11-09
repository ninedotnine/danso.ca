---
title: simple, type-safe string formatting in Haskell
tags: haskell, programming
description: without even dependent types
published: 12021-11-08
---

Years ago, on a distant website, Lennart Augustsson [responded to a question](https://stackoverflow.com/questions/7828072/how-does-haskell-printf-work) about how `printf` can work in Haskell, and whether it is type-safe:

> You can only get a type safe printf using dependent types. 

> --- augustss

augustss ranks among the most elite of Haskell legends, so if he says so, then...  hm.

*Challenge accepted.*

## the `FmtSpecifier` type

Instead of a naïve `String`, use a more sophisticated data type to encode the format. Each conversion specifier (those things beginning with `%`, like `%i` and `%s`) becomes a data constructor, with its argument being the value to print.

```haskell
data FmtSpecifier = FmtStr String
                  | FmtChar Char
                  | FmtInt Int
                  | FmtFloat Double
```

We will use a function that can convert a `FmtSpecifier` to a `String`:

```haskell
convert :: FmtSpecifier -> String
convert = \case
    FmtStr s -> s
    FmtChar c -> [c]
    FmtInt i -> show i
    FmtFloat n -> show n
```

## the `sprintf` and `printf` functions

I rarely want to convert only one format specifier into a string --- normally I want to combine multiple. `printf` therefore takes a list of `FmtSpecifier`s!

```haskell
sprintf :: [FmtSpecifier] -> String
sprintf = (>>= convert)

printf :: [FmtSpecifier] -> IO ()
printf = sprintf <&> putStr
```

Et voila:

```haskell
report :: String -> Int -> IO ()
report name number =
    printf [ FmtStr name
           , FmtStr " is player "
           , FmtInt number
           , FmtChar '\n' ]
```

An invocation like `report "Gi-hun" 456` will happily output `Gi-hun is player 456`.

It's a little wordier than `"%s is player %i\n"`, but it's guaranteed not to ever segfault, which is nice.[^percent]

## how to left-pad (without breaking the entire web)

One of the features of `printf` is that the caller can adjust how the values are printed, such as by specifying a maximum or minimum width (i.e. number of characters).

Not to appear incomplete, we demonstrate how to left-pad a string to a minimum width. Add another data constructor to our `FmtSpecifier` type:

```haskell
-- data FmtSpecifier = ... 
                  | FmtPaddedFmt Int Char FmtSpecifier
```

and tell the `convert` function how to handle this case:

```haskell
-- convert = \case ...
    FmtPaddedFmt min_len char fmt ->
        if min_len > len
            then replicate (min_len - len) char <> str
            else str
          where
            str = convert fmt
            len = length str
```

If the formatted *thing* isn't as long as the required width (`min_len`), then we prepend[^replicate] as many of the character `char` as we need until it is!

```
ghci> printf [FmtPaddedFmt 8 '0' (FmtInt 1729)]
00001729
```

## the many looks of floating-point numbers

Somebody who wanted to add all the formatting features of decimal numbers (showing the `+` sign, using scientific notation, and so on) might begin with a record type encapsulating all our needs[^bool-blindness]:  

```haskell
data FmtFloatQualifiers = FmtFloatQualifiers {
      show_sign :: Bool
    , show_decimal_point :: Bool
    , scientific_notation :: Bool
    , precision :: Int
    }
```

And then, just as above: 

```haskell
-- data FmtSpecifier = ... 
                  | FmtQualFloat FmtFloatQualifiers Double
```

```haskell
-- convert = \case ...
    FmtQualFloat quals n -> fmt_float quals n
        where
            fmt_float :: FmtFloatQualifiers -> Double -> String
            fmt_float = undefined
```

Adding all these features is orthogonal to the purpose of this post, and so the definition of `fmt_float` is left as an exercise for the reader.

---

There you are! In about 30 lines we were able to do the (supposedly) impossible.

Okay, okay, I know that I haven't outsmarted augustss with this post. There is nothing here that would surprise him in any way. That opener was a bit cheeky of me.

A later comment even clarifies that it can be done this way "if you choose a more informative type than String for the format."; augustss apparently found this so obvious that he didn't even need to reply.

Nonetheless, I thought it was a fun demonstration. The full code is [available on GitLab](https://gitlab.com/danso/blog-extras/-/blob/main/TypedPrintf.hs).

[//]: # (footnotes)

[^percent]: And printing one percent sign doesn't require *two*, what a bonus!

[^replicate]: The `replicate` function produces the empty list when given a number of zero *or less*, so checking that the length we need is more than zero is not entirely necessary. I always feel that trying to make a list with negative length is something that a strong, static type system should prevent.

[^bool-blindness]: Optionally, using custom types instead of (particularly) `Bool` values would make their [meanings more explicit](https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/).
