---
title: fromMaybe is Just a fold
tags: haskell, programming
description: I realized something neat today 
published: 12021-07-29
warning: This post assumes a basic familiarity with the Haskell programming language. I will make the examples as accessible as feasible.
---

`fromMaybe` is a useful function. `Maybe` is not guaranteed to hold a value, but you can always get one by providing a default to fallback on:

```haskell
fromMaybe :: a -> Maybe a -> a
fromMaybe dfault m_x = case m_x of
    Just x -> x
    Nothing -> dfault
```

At the interactive Haskell shell, it behaves exactly as intended:

```
ghci> fromMaybe "default" (Just "a string")
"a string"
ghci> fromMaybe "default" Nothing
"default"
```

It's a bit wordy. If I am working with `Maybe`-heavy code, I sometimes alias this function to the operator `//`.[^tradition]

```haskell
x // y = fromMaybe y x
```

or the point-free style:

```haskell
(//) = flip fromMaybe
```
And now:

```
ghci> :type (//)
(//) :: Maybe c -> c -> c
ghci> Just "one little fox" // "no animals here!"
"one little fox"
ghci> Nothing // "no animals here!"
"no animals here!"
```

So concise!

## enter `Either`

Well today I was converting some `Maybe` code to use `Either ErrorCode`. This is not difficult --- the strong type system makes it a pretty mechanical process. I replaced `fromMaybe` with this definition of `fromEitherE`[^fromright]: 

```haskell
fromEitherE :: a -> Either e a -> a
fromEitherE dfault e_x = case e_x of
    Right x -> x
    Left _ -> dfault
```

Normally this would entail replacing the calls to `fromMaybe` all over the place, but since I had been using the `//` alias everywhere, that was all I needed to change:

```haskell
(//) = flip fromEitherE
```

To show that it works:

```
ghci> Right "my cool home page" // "server error"
"my cool home page"
ghci> Left 418 // "server error"
"server error"
```

We get the `Right` value if there is one, and if not, ignore the error code and use the provided fallback value.

Sure, this works, but somehow I am dissatisfied. I expected to find a polymorphic solution that can handle both cases cleanly. After all, look at the similarities in their types:

```haskell
fromMaybe   :: a -> Maybe    a -> a
fromEitherE :: a -> Either e a -> a
```

I wondered whether there were any other functors which exhibit the same pattern. 

## lists

We can imagine extracting the first element of a list, or using a fallback if the list is empty![^head]

```haskell
fromList :: a -> [a] -> a
fromList dfault l = case l of
    first:rest -> first
    [] -> dfault

(//) = flip fromList
```

```
ghci> [] // "no trains :v("
"no trains :v("
ghci> ["3pm"] // "no trains :v("
"3pm"
ghci> ["3pm","7pm"] // "no trains :v("
"3pm"
```

It is in this form that a solution becomes the most clear. We are reducing a list to a single element, an operation which shares its name with a certain bread-making technique.

## folding

A `fold`, in the Lisp tradition, is a function which takes a combining function, a starting value, and a list. It uses the function and starting value to walk through the list, accumulating as it goes along.

```haskell
fold :: (a -> a -> a) -> a -> [a] -> a
fold f start list = recurse list
    where
        recurse l = case l of
            [] -> start
            first:rest -> f first (recurse rest)
```

```
ghci> fold (+) 0 [1,2,3,4,5]
15
```

We could use `fold` to implement our `fromList` function --- we just need a function which always returns its first argument!

```haskell
const :: a -> b -> a
const x y = x
```

```
ghci> fold const 0 [1,2,3]
1
ghci> fold const 0 []
0
```

But lists are far from the only structures that we can fold. The `Foldable` type-class exists to capture the pattern of types which can be folded in some way using a function. Common examples include lists, trees, and sets.

The general version is called `foldr` and looks like this:

```
ghci> :info Foldable
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
  length :: t a -> Int
-- and many more...

instance Foldable Set -- Defined in ‘Data.Set.Internal’
instance Foldable [] -- Defined in ‘Data.Foldable’
instance Foldable NonEmpty -- Defined in ‘Data.Foldable’
instance Foldable Maybe -- Defined in ‘Data.Foldable’
instance Foldable (Either a) -- Defined in ‘Data.Foldable’
```

Wait. `Instance Foldable Maybe`? Yes!

```
ghci> length Nothing
0
ghci> length (Just undefined)
1
ghci> foldr (+) 2 (Just 2)
4
```

It's true! We can fold both `Maybe`s and `Either a`s. This suggests a polymorphic solution to our puzzle:

```haskell
(//) :: Foldable f => f a -> a -> a
(//) = flip (foldr const)
```

```
ghci> Just "ok" // "otherwise"
"ok"
ghci> Right "ok" // "otherwise"
"ok"
ghci> ["ok"] // "otherwise"
"ok"
```

And there was much rejoicing.

## but...

One more thing. 

`(//)` does not make sense for every `Foldable`. `NonEmpty` lists, for example, are guaranteed to hold at least one value. If we want to get the first value out of a `NonEmpty`, we always can! There's no need for an "otherwise" value.

We can avoid `(//)` on such types by defining our own sub-class of `Foldable`:

```haskell
class Foldable f => Optional f where
    (//) :: f a -> a -> a
    (//) = flip (foldr const)

instance Optional Maybe
instance Optional (Either e)
instance Optional []
```

## cool, that's it

This post was inspired by a discussion in the #haskell irc channel on [libera.chat](https://libera.chat), with special thanks to geekosaur and hpc. I hope you found it interesting too.

All the code examples above are available on [GitLab](https://gitlab.com/danso/blog-extras/-/blob/main/Optional.hs) and [GitHub](https://github.com/ninedotnine/blog-extras/blob/main/Optional.hs).

[//]: # footnotes

[^tradition]: I would prefer to use `||`, in the style of Unix shell. Unfortunately, Haskell copied `||` from the C tradition and uses it for Boolean disjunction. If you really want to, you could `import Prelude hiding ((||))` and then redefine `(||)` to whatever you like. In this post, we will instead use `//` as a tribute to Perl. 

[^fromright]: Inconsistently called `fromRight` in the [base](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Either.html#v:fromRight) package.

[^head]: This function is commonly called "head", except that the `head` function in [base](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:head) is partial and will crash your program if passed an empty list.
