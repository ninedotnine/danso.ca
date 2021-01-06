---
title: "happy-space: a whitespace-sensitive calculator"
tags: hacking, math, programming
description: Is it terrible, or a bit cute?
published: 12020-12-30
---

I've never been really satisfied with calculator programs. I can't explain why.

I suppose the standard utility for this (on Unix) is `bc`, but when I once briefly wanted to use it, I discovered it to be basically an entire complex programming language that I didn't understand. 

I only want to write `1+2` and see a `3` pop out.

Seeing nothing better, I've been using Python and GHCi for this purpose. They still do *way* more than necessary, but at least they're familiar.

### But that's a mislead 

I'm sure a good, minimal calculator exists. That's not the point. The point was that I hadn't found one, so I was going to make my own, and it would be better than all the others. It would be simple and intuitive, and would do nothing other than calculate expressions. Most importantly, it would have one very special feature. 

But before I get ahead of myself:

## What is an expression?

An arithmetic expression, for my purposes, refers to numbers separated by infix operators, such as `1+2` or `2*x/5`. 

Most programming languages do not evaluate expressions the same way we read English, from left to right. There is the notion of *operator precedence* --- some operators need to be evaluated before others.[^before] Operators with highest precedence are evaluated first, and only then left-to-right. As an example, `2+3*5` evaluates to `17` (not `25`) because multiplication is evaluated before addition. 

The large majority of programming languages use precedence rules similar to the convention for arithmetic, although there are some that always evaluate from left to right (e. g. Smalltalk) and others where the very concept of operator precedence makes no sense (Lisp-family languages).

The operators I am interested in supporting are addition (`+`), subtraction (`-`), multiplication (`*`), division (`/`), modulo (`%`), and exponentiation (`^`). As usual, I assigned `^` the highest precedence, followed by `*`, `/`, and `%`, with `+` and `-` having the lowest.

Additionally one *prefix* operator for negation would be nice. Since `-` is already in use, I chose `~`. 

There is also the idea of *operator associativity*. Associativity provides the answer the question: should `x-y-z` be understood as `(x-y) - z` or `x - (y-z)`? I chose a simple left-to-right associativity for all operations[^power].

## What makes `happy-space` special?

`happy-space` does one thing which is unique: it understands whitespace-sensitive expressions. This means that the various whitespace characters (space, tab, newline) have semantic meaning and can actually change the value of the evaluated expression.

Whitespace-sensitive grammars have existed for decades; Python is a well-known example of a language which includes meaningful whitespace. `happy-space` itself is written in Haskell, another such language.

But this is the first time (to my knowledge) that whitespace has been significant in a language for *expressions*.[^wall]

Specifically, by including spaces around an operator, you can lower that operator's precedence so that it is evaluated after an non-spaced operator.

Let's see what we can do with it:

```
> 3 + 6 / 3
5
```

Expected --- division has higher precedence than addition, so `3 + 6 / 3` is `3 + 2`, which is `5`.

```
> 3 + 6/3
5
```

Since division already had higher precedence than addition, this changes nothing.

```
> 3+6 / 3
3
```

Wow! Because the `3+6` is grouped by the way I used spaces, the addition is performed first!

I have cleverly termed this *whitespace operator precedence*.

## New rules

Effectively, by omitting spacing around operators, you can recreate the effect of parentheses without using any. The whitespace precedence rule allows me to write expressions that *look like what they mean*.

It does raise problems, however. What does `(3 + 6)/3` mean? Or `3+ 6 / 3`?

The first example is actually no problem at all. Because parentheses have to match, there's no way to get them wrong, unless you completely forget to pair them. There are no situations where parens and spacing can conflict with each other.

The second is tougher; should the addition be given higher precedence if it is only spaced on one side? My solution is to treat this the same was as something like `1+*)` --- as an invalid input. It has no meaning. While most expression languages will let you be pretty sloppy, spaces matter here, so you cannot be inconsistent with it. Try to put a space after an operator, but not before, and:

```
> 1+ 2
"input" (line 1, column 4):
unexpected whitespace after `+`
```

Likewise:

```
> 1 +2
"input" (line 1, column 4):
unexpected "2"
expecting space after `+`
```

`happy-space` defensively rejects expressions when it cannot be sure what you intended.

## In my defence

As my esteemed colleague put it:

> I can see this causing many bugs.[^who]

Is this concern justified? Maybe. There are three possible cases:

### Expressions with no whitespace

An expression such as `1+2*3/4` is not affected at all by the whitespace precedence rule. An expression without spaces (or where all operators are evenly spaced) will behave exactly as anyone expects.

### Expressions with whitespace spread disgustingly at random

If you write an expression such as `1+2 /3` or `4 *5+6` or even simply `(7 )` then

* you have no class,
* you will not get a *wrong* answer.

Happy-space rejects your so-called "expressions" if the whitespace on both sides of an operator is not equal, or if your parentheses are too ugly. To deserve a result, you need to clarify your meaning, as it should be.

### Expressions with misleading whitespace, leading to surprising results

If you write an expression such as `2+4 / 2` and you believe that the result *should* be `4`, then

* you are a significantly worse person kind of person than the previous guys,
* I hate you and hope that you choke on your unexpected result, and
* you have a valid point. But your taste is awful.[^michigan]

All programming languages that I'm familiar with apply the usual order-of-operations rules and either disallow whitespace or ignore it completely. Because of this tradition, it is possible to write `x+y / z` with the expectation that `y / z` will be evaluated first.

However, I contend that this is not a new problem, and that whitespace precedence does not make it worse. It is already possible to be misled by spacing. This is the only case where whitespace precedence can produce a surprising result, but it will only do so if your use of spacing *looks* wrong.

Let us consider the expression `w+x / y+z`. In any conventional (i. e. with usual precedence and whitespace anarchy) expression language, this would be parsed as `w + (x/y) + z`. Division comes before addition, regardless of how it looks.

The programmer who wrote this expression almost certainly did not intend that. I believe they had in mind `(w+x) / (y+z)`; a quotient which would be translated onto paper using a long horizontal line. This programmer has either forgotten about the order of operations or is unaware of them.

With whitespace precedence, `w+x / y+z` means what it appears.

The question isn't only "does whitespace precedence cause bugs?", but "does whitespace precedence cause *fewer* bugs than without it?". I have zero data to back this up, but I suspect that people are tripped up more often by the rules as they are widespread than would be by my whitespace rule.

## That's it

The `happy-space` code isn't beautiful. It's a bit long and redundant in places. I'm nonetheless pleased with the result: it feels snappy and lets me say what I mean without (m)any parentheses.

If you want to use this, then you are welcome download the statically-linked binary from [GitHub](https://github.com/ninedotnine/happy-space/releases) or you can clone the repository and build it yourself. I compile it with GHC 8 on Arch Linux and have made no effort to test it on any other platform.

Happy-space and its code are made freely available under the terms of the GNU AGPL. Bug reports and contributions are welcome.

[//]: # footnotes

[^before]: I have written about the [order of operations](/blog/order-of-operations/) before.

[^power]: Many programming languages choose to make the exponent operator associate right-to-left. I don't think it's worthwhile to introduce this complexity for only one operation, especially since it's dubious that right-to-left is even preferable.

[^wall]: Although we came up with this idea independently, Lewis Wall wrote a [blog post](http://wall.org/~lewis/2013/10/25/whitespace-precedence.html) describing a strikingly similar rule.

[^who]: Actually, *colleagues*. Multiple people said these exact words.

[^michigan]: "Gross indecency" is still an actual crime in Michigan, apparently.

