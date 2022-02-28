---
title: understanding higher-kinded types
tags: math, programming
description: a little type theory
published: 12022-02-27
---

*Kinds* are the types of types.

Okay, let me take a step back:

## proper types

*Values* have *types*.
Some typical types are `Integer`, `Bool`, and `String`. 
Values of type `Integer` include `0`, `1`, `42`.

Just as values can be grouped into types,
types can be grouped into kinds.

`Bool` and `Integer` are among the simplest types.
We say that these types have kind `Type`.[^type]
`Type` is the kind containing all *populated* types ---
those types which have values.

Types of kind `Type` are the only types
which can be thought of as sets of values,
like `{true, false}` or `{..., -2, -1, 0, 1, 2, ...}` .
Populated types are also called *proper types*.
[^void]

## first-order types

There are also values of type `List(Integer)`[^angles]
such as `[1,2,3]` and `[]` ,
so `List(Integer)` has kind `Type`.
But there are no values of type `List`!

`List`[^array] is not a proper type.
In a certain sense, it needs another (proper) type to complete it.
We can think of `List` as a *function*
whose [domain] and [codomain] are both proper types.
`List` has kind `Type -> Type`.

Other examples of types with kind `Type -> Type`
are `Set` and `Maybe`[^optional].
Together, these are *first-order* types.

`Pair`, `HashMap`[^dict], and `Either`[^result]
are not proper types either ---
they are functions
whose domains are pairs of types
and whose codomains are types.
In other words, they have kind `(Type,Type) -> Type`.
[^functions] 

There are no values of type `HashMap`,
nor any of type `HashMap(String)`,
but there are values of type `HashMap(String,Integer)`
such as `{ "hello": 2 , "hi": 3 }`.
[^currying]

Types which require any number
of other proper types as arguments
to form proper types themselves
are *first-order* types.
First-order types effectively allow us
to abstract over proper types.

## beyond first-order

But first-order types do not allow us to abstract
over other first-order types.
There is no such thing as a `List(List)`.

Believe it or not, there are some (useful!) types
which require first-order types to complete them.
These types have kinds such as `(Type -> Type) -> Type`
and are called *higher-order* or *higher-kinded* types.

Examples include [Foldable], [Traversable], [Functor], and [Monad].

---

Kinds and first-order types 
can help us understand type-classes (or generics) 
as a logical extension of the type system.

Higher-kinded types take that a step further 
and include first-order types in our generics.
They provide the means to abstract over types
which themselves abstract over types.

*With thanks to [hboo]
for revising an earlier draft of this post.*

[//]: # footnotes

[^type]: 
    If you find it confusing that `Type` is the name
    of the kind containing all the types,
    remember that `Integer` is the name
    of the type containing all the integers!

[^void]: 
    There's a minor technical difference 
    between a proper type and a populated type.
    A populated type has kind `Type`
    and there is at least one value of that type.
    A proper type meets the first requirement,
    but not necessarily the second.

    `Void` is a type with no values (an empty set),
    but it still has kind `Type`.
    It is a proper type that is not populated.

[^angles]: spelled with angle brackets in many languages, as in `List<Integer>`

[^array]: or `Array`

[^optional]: or `Optional`

[domain]: https://en.wikipedia.org/wiki/Domain_of_a_function

[codomain]: https://en.wikipedia.org/wiki/Codomain

[^dict]: or `Dict`

[^result]: or `Result`

[^functions]: 
    Also in this gang is `->` (function) --- 
    it has kind `(Type,Type) -> Type` 
    because it requires two types to complete it. 
    In other words, there are no values of type `->`, 
    nor are there values of type `String ->` or `-> Integer`. 

    There *are* values of type `String -> Integer`, 
    such as the `length` function 
    where `length(s)` is the number of characters in `s`.

    Expressing functions using the `->` syntax 
    can hide the fact that `Function` is really just a type
    of kind `(Type,Type) -> Type` like any other. 
    You can think of `String -> Integer`
    as `Function(String,Integer)` if you prefer.

[^currying]: 
    In functional programming languages, 
    these kinds are often curried.
    Instead of `(Type,Type) -> Type`, 
    `HashMap` might have kind `Type -> (Type -> Type)`.
    A value would have type `HashMap(String)(Integer)`.

[Foldable]: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Foldable.html#t:Foldable

[Traversable]: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Traversable.html#t:Traversable

[Functor]: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Functor.html#t:Functor

[Monad]: https://hackage.haskell.org/package/base-4.16.0.0/docs/Control-Monad.html#t:Monad

[hboo]: https://heatherbooker.github.io/blog/
