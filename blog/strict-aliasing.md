---
title: Strict aliasing in GCC
tags: c, programming
description: How often do you defend your compilers?
published: 12019-12-22
---

A certain angry[^nvidia] but otherwise well-regarded git once posted a bit of a rant[^rant] about a tool that his project depended heavily on.

This post is not quite a response to that, since (beside being many years late) as far as I know, it behaved completely differently then than it does now.[^ancient]

However, the message does still get quoted, too often.

### some context

The C language standard is clear:

> If an object has its stored value accessed other than by an lvalue of an allowable type, the behavior is undefined.[^c99]

In other words, if you write code like this[^this]:

```
int f(void) {
    int *ip;
    double d = 3.0;
    ip = &d;
    return *ip;
}
```

then your program has no defined behaviour; it is meaningless[^undefined]. A conformant C implementation may interpret this code however it likes. In fact, since any behaviour is correct behaviour, a clever implementation can act as if this code code will never even be run --- because if that *does* happen, anything the implementation does will be correct.

Was this a good decision on the part of the C standardisation committee? Arguably not, but that's beside the point. This is C, and if you write C code, then this is a matter that you need to understand, or it will get you sooner or later.[^sooner]

### gcc

One of the most popular ever implementations of the C language is called GCC --- the GNU C Compiler (or GNU Compiler Collection)[^gcc]. GCC is an amazing piece of technology and an absolutely massive body of software depends on it. Not only does it comprehensively and correctly[^correct] implement the C language according to the standard, it does so efficiently and it can even perform optimizations on C code. A trivial optimization would be simplifying expressions, such as `3+4` to `7`; more complex optimizations include memory reuse and reordering instructions. All this, and it is made available *for free* and comes with all the legal protections of a GNU free software license.[^cpp]

The GCC developers agreed that the above example was a particularly subtle C language trap to avoid, and so they introduced a compiler setting --- `-fno-strict-aliasing` --- which instructs the compiler to be gentle and assume that the code might be... mistaken. Its counterpart, `-fstrict-aliasing`, specifically tells GCC that you are confident that you haven't written any such bad code, and that it can use that assumption in making optimizations.

### default settings

Basically, with `-fno-strict-aliasing`, you are advising the compiler that you *might* have written some incorrect code, and to please be defensive with the optimizations it performs regarding aliasing.

Since this broken code has no required behaviour, both aggressively optimizing and cautious non-optimizing are compliant with the C language standard. In other words, GCC compiles C correctly per the standard with or without this option set.

However, code that only works with GCC with `-fno-strict-aliasing` is not correct C, and will likely be broken with a different correct C implementation.

Many people feel that `-fno-strict-aliasing` ought to be the default setting when compiling with GCC. I have news for those people: *it is*.

### a typical sequence of events

C is difficult to write correctly. We do our best, but sometimes mistakes creep in. That's okay: GCC is careful and seems to generally do what we want, even when we fail to express our intent properly. We don't even notice when our code is incorrect, and quickly we come to depend on GCC's clairvoyance.

But eventually, our program starts to grow big and clunky. We notice the start-up time. It doesn't respond instantly to our input. That's when, in the noble pursuit of faster execution, we enable optimizations with a setting like `-O3`.

> The `-fstrict-aliasing` option is enabled at levels `-O2`, `-O3`, `-Os`.[^man]

Uh oh --- somebody failed to read the manual. The program crashed, the clients are angry, and the server room is on fire.

At this point, it is easy to assign blame to the compiler, especially when the aforementioned angry git's message can be cited.

### -fwrapv

Permit me a little detour, for a moment --- I would like to provide another example.

If, in C, I try to store the value of the expression `INT_MAX + INT_MAX`[^intmax] into an object of type `int`, what should happen?

The C language standard says plainly that overflowing the maximum bound of an integer type is undefined behaviour. A machine that does anything (or nothing!) is therefore compliant with everything the specification demands.

In an obvious case like this, the compiler *could* statically determine that the result would overflow. It could stop in its tracks and advise me that I'm doing something silly. However, it is not required to do this.

```
#include <limits.h>

int main(void) {
    return INT_MAX + INT_MAX;
}
```

GCC doesn't actually *prevent* me from doing this, but it does alert me that something is amiss:

```
overblown.c: In function 'main':
overblown.c:4:20: warning: integer overflow in expression of type 'int' results in '-2' [-Woverflow]
    4 |     return INT_MAX + INT_MAX;
      |                    ^
```

One possible behaviour is that the compiler could define a requirement for what will happen. To do this would go above and beyond what the C language standard requires.

GCC, for which "above and beyond" is basically the modus operandi, offers the option to define the semantics of integer overflow to wrap-around using twos-complement. All you need to do is pass `-fwrapv`.[^fwrapv] Thanks GCC!

### the point

GCC is not to be blamed[^blame] for the consequences of C's strict aliasing rules --- it does the correct thing in all cases. It correctly implements C, and, by default, even takes extra care when presented with broken code.

Then users complain that GCC does something unsafe with their broken program, after telling GCC to apply the standard's aliasing rules in the strictest possible way to produce faster code.

There are two parties that can reasonably be blamed here: the programmer who wrote the incorrect program, and the standardisation committee that decided to make the language unsafe. GCC is not at fault.

Turning on `-fno-strict-aliasing` is a perfectly reasonable decision, especially if you are not confident that your program is correct.

[//]: # (footnotes)

[^nvidia]: [Youtube link](https://www.youtube.com/watch?v=iYWzMvlj2RQ)
[^rant]: [Linux Kernel Mailing List](https://lkml.org/lkml/2009/1/12/369)
[^ancient]: Though according to the documentation, that was not the case. For example, `-fstrict-aliasing` is documented in the [manual for GCC 2.95.3](https://gcc.gnu.org/onlinedocs/gcc-2.95.3/gcc_2.html#Optimize%20Options), released 18 years ago.
[^c99]: C99 section 6.5, paragraph 7.
[^this]: Don't worry if you don't understand what this means, since it doesn't mean anything.
[^undefined]: [What Every C Programmer Should Know About Undefined Behavior](http://blog.llvm.org/2011/05/what-every-c-programmer-should-know.html)
[^sooner]: In fact, you've probably already been gotten and might not know it yet.
[^gcc]: [GCC, the GNU Compiler Collection](https://gcc.gnu.org/)
[^correct]: As far as I can tell.
[^cpp]: Regrettably, it can also compile C++. Nobody is perfect.
[^man]: [GCC online documentation](https://gcc.gnu.org/onlinedocs/gcc-9.2.0/gcc/Optimize-Options.html#index-fstrict-aliasing)
[^intmax]: `INT_MAX` is the maximum value for an object of type `int`.
[^fwrapv]: In fact, Linux [does this](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=68df3755e383e6fecf2354a67b08f92f18536594).
[^blame]: It appears Linus Torvalds himself has reconsidered who deserves the blame since writing the original message. Last year, he posted [a slightly calmer message](https://lkml.org/lkml/2018/6/5/769) in which he describes `-fstrict-aliasing` as "to undo the braindamage that that piece of garbage C standard imposes".
