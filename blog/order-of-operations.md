---
title: The correct answer to `8 ÷ 2(2+2)` is 1
tags: education, internet, math, semantics
description: Finally, I put an end to this misconception.
published: 12019-08-05
---

This is a tricky little arithmetic problem that has been firing up conversations (fights) online.

It says something about the sorry state of our education system that so many Twitter users believed the answer to be 16. In this post, I will demonstrate conclusively that the correct answer is in fact 1[^fact].

## Method

We take our original expression:

$8 ÷ 2(2+2)$

Begin by evaluating everything within parentheses first.

$8 ÷ 2(4)$

Next, we evaluate the call to the function `2`[^2] passing in the argument `4`:

$8 ÷ 8$

The last step is a trivial division.

$1$

QED.

## Okay, to get $\mathbb{R}$ for a moment[^real]

Obviously, this was silly. Or was it?

*Of course* function application isn't what anybody means when they write `2(4)`. But why not? `a(b)` means multiplication, you say --- but isn't `a(b)` *also* the syntax we commonly use in mathematical notation to express function application?

Let me ask you this: how do you read `1/10x`? I find it perfectly reasonable to interpret this either as `(1/10) * x` or as `1 / (10*x)`. If there are two reasonable interpretations, then the original expression was unclear.

The point of this question is that the notation is deliberately ambiguous. To consider it seriously is somewhat of a waste of time. It is the responsibility of the writer to express their intention unambiguously, and, having failed to do so, the result of `8 ÷ 2(2+2)` is [unspecified](https://en.wikipedia.org/wiki/Unspecified_behavior).[^lamb]

## Understanding *PEMDAS*

There is an actual lesson to be learned here: blindly applying the "rules" of arithmetic order is not the way.

I tend to think that mnemonics such as "please excuse my dear Aunt Sally" actually do a disservice to math students. Using this memory system to remember the names and order of the planets is totally justified --- this information is *completely arbitrary*. The order of operations is not arbitrary, at all, and the implication that it might be is undesirable.

So what is the order of operations? It is a convention, like any other language. It hardly matters much *what* the convention is --- we can communicate as long as we can agree to it.

"Always evaluate from left to right" would have been a perfectly acceptable convention to settle on. Equally good would have been "always right to left", which would have been more consistent with the Arabic from which our numerals are derived.

Whatever convention is used, we would at times have a need to tell the reader that a particular subexpression needs to be evaluated in a different order. Thus, parentheses. You don't need PEMDAS to remember that parentheses come first, because coming first is *the entire point* of parens. Besides, since they always come in pairs, there's basically no other way to parse them.

At some point, by someone, it was decided that it's very often useful to perform the "larger" operations first --- meaning exponentiation, then multiplication/division, and finally addition/subtraction. The decision to prioritize these "greater" operations first was a practical one, intended to reduce the number of parentheses. A convention was born: *most powerful* operations first.

Just as multiplication is repeated addition, and exponentiation is repeated multiplication, we have [tetration](https://en.wikipedia.org/wiki/Tetration) which is repeated exponentiation. If we commonly used tetration, we would have a widely-agreed-on operator for it, and its precedence would presumably be higher than exponentiation.

It is necessary to understand that multiplication and division are inverses of one another, as are addition and subtraction. They are, in a sense, the same thing, and to prioritize one over the other would be arbitrary.[^miles]

This also implies that logarithmation[^logarithmation], being the inverse of exponentiation, should share its precedence. If we had a special notation for logs, it certainly would, but the notation we use ($\log_x (y)$ or $\log (x,y)$) already prevents ambiguity.[^logs]

A student who understands this series of "levels" of operations[^hyperoperations], who understands that parentheses can only do one thing, and who understands the duality between addition/subtraction and multiplication/division is a student who has no need for PEMDAS.

[//]: # (footnotes)

[^fact]: Or, in Haskell syntax, `fact (fact 1)`. Or `fact . fact . fact $ 1`. Also correct is $1^1$.
[^2]: `2`, naturally, was earlier defined to be $\lambda x . x+x$. $2^2$ is another equivalent application of the `2` function, provided the input is always `2` --- but why would you `2` anything else?
[^real]: Can you tell this post was partly an excuse to have fun with Pandoc?
[^lamb]: Evelyn Lamb wrote an [excellent piece](https://blogs.scientificamerican.com/roots-of-unity/the-only-way-to-win-is-not-to-play-the-game/) in Scientific American that says everything else that I might want to say here.
[^miles]: Mathematical illiterate Frank Miles [believes](https://www.foxnews.com/tech/viral-math-problem-baffles-many-internet) that *PEMDAS* and *BODMAS* are actually *different rules* for ordering operations rather than simply different mnemonics.
[^logarithmation]: Inarguably a useful word.
[^logs]: For those who like to write their logarithms without using any parens, as in $log_x y$, I encourage you to consider the meaning of $log_2 8(2+2)$.
[^hyperoperations]: The hyperoperation sequence.
