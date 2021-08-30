---
date: 2021-08-19T15:26:15Z
lastmod: 2021-08-26T15:26:15Z
publishdate: 2021-08-23T15:26:15Z

title: Hugo Maximal Minimalist  Theme
description: How to think about Monads
---

# Hugo Maximal Minimalist  Theme

> bespoke technical documentation.

## How to think about Monads

Initially, Monads are the biggest, scariest thing about Functional Programming and especially Haskell. I've used monads for quite some time now, but I didn't have a very good model for what they **really** are. I read Philip Wadler's paper [Monads for functional programming](http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf) and I still didnt quite see the pattern.

It wasn't until I read the blog post [You Could Have Invented Monads! (And Maybe You Already Have.)](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html) that I started to see things more clearly.

This is a distillation of those works and most likely an oversimplification in an attempt to make things easier to understand. Nuance can come later. What we need when first learning something is a simple, if inaccurate, model.

This document assumes a beginner's knowledge of pure functional programming and Haskell with some brief encounters of Monads, e.g. [Functors, Applicatives, And Monads In Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html) or [A Fistful of Monads](http://learnyouahaskell.com/a-fistful-of-monads).

## Pure Composition

Take the following pure functions:

{{< highlight haskel >}}
f :: Float -> Float
f x = x + 10

g :: Float -> Float
g y = y * 100 
{{< / highlight >}}

They are simple enough. And as is so common in functional programming, I'm going to combine these two function using *functional composition*.

```lang=hs
h :: Float -> Float
h = g . f
```

Pictorially:

```
   +-----+    +-----+
-> |  f  | -> |  g  | ->
   +-----+    +-----+
```

The power of composing smaller, simpler things to produce larger, more complex things can be seen in nature (atoms, molecules, amino acids, protiens, etc.) and programming (statement, function, module, program), and so on.

There is no argument that this operation is advantageous.

## Pure Composition with Side Effects

Now let's take our beautiful, pure functions and let's ruin them with some impurity. Let's say we want to log our operations, i.e. make `Debuggable` versions of our functions.

Here's some **impure** javascript code to illustrate:

```js
var log = '';
const f = x => { log += 'added 10\n'; return x + 10; };
const g = y => { log += 'multiplied by 100\n'; return y * 100; };
const h = z => g(f(z));
```

Here the **global** variable `log` is appended to each time `f` or `g` are called, i.e. these functions produce a **side effect**.

To do this in a pure functional language requires a very different tact, however. In pure functional programming, everything is passed into a function and everything that function produces is passed out.

To accomplish a similar task with pure functions requires us to return BOTH the result of the computation AND all of its side-effects.

Here's some Haskell code to accomplish just that:

```hs
f' :: Float -> (Float, String)
f' x = (x + 10, "added 10\n")

g' :: Float -> (Float, String)
g' y = (y * 100, "multiplied 100\n")
```

Now we need to compose these 2 functions but we cannot use `(.)`, so we are forced to write:

```hs
h' :: Float -> (Float, String)
h' z = 
  let (r, s)  = f' z
      (t, s') = g' r in
  (t, s ++ s')
```

Pictorially:

```
   +-----+                                  +-----+
-> |  f' | -> (computation, side-effect) -> |  g' | ->
   +-----+                                  +-----+
```

UGH!!! This code is painful to write each time. Imagine the work we'd have to do if we wanted to compose more than 2 functions.

What would be great is if we had a compose function that would work with `f'` and `g'` and produce `h'` simply and elegantly.

It's not obvious how one goes about doing this. So let's start with the standard compose function, `(.)`:

```hs
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) g f x = g (f x)
```

This won't work for us because we don't have functions of type `b -> c` or `a -> b`, instead we have functions of type `Float -> (Float, String)` or more generally, `b -> (c, String)` and `a -> (b, String)`.

What we want is a compose function that will allow us to compose our `Debuggable` functions:

```hs
compDebuggable :: (b -> (c, String)) -> (a -> (b, String)) -> (a -> (c, String))
compDebuggable g' f' x =
  let (r, s)  = f' x
      (t, s') = g' r in
  (t, s ++ s')
```
Compare `compDebuggable`'s signature with `(.)`'s. `c` and `b` have been replaced with `(c, String)` and `(b, String)` respectively.

Notice how `compDebuggable` **manages** the `side-effect` for us. *(This will be important later.)* It squirrels them away into variables until it's time to combine them and then it does so returning the result.

With this function we can compose functions that produce `String` as a `side-effect`:

```hs
h' :: Float -> (Float, String)
h' = g' `compDebuggable` f'
```

which looks very similar to the `no side-effect` counterpart:

```hs
h :: Float -> Float
h x = g . f
```

## Composing Side Effects with No Side Effects

We can compose `side-effect` functions with other `side-effect` functions. We can also compose `no side-effect` functions with other `no side-effect` functions. But, how do we compose them with each other?

There are 2 possibilities.

We could convert the `side-effect` function into a `no side-effect` function, but then we'd lose all of the `side-effects` which defeats the whole purpose.

Or we convert the `no side-effect` function into a `side-effect` function, letting us keep our `side-effects` and leverage a ton of `pure` functions.

Since a `Debuggable` function appends to the "log", we want our `no side-effect` function to have **no** impact on the log. Therefore, we return an **empty string** as the `side-effect` of any `no side-effect` function that we convert:

```hs
mkFuncDebuggable :: (a -> b) -> (a -> (b, String))
mkFuncDebuggable p x = (p x, "")
```

Now we can compose a `no side-effect` function, `nse` with a `side-effect` one, `r'`:

```hs
nse :: Float -> Float
nse x = x + 1

r' :: Float -> (Float, String)
r' = mkFuncDebuggable nse `compDebuggable` f'
```

Pictorially:

```
   +-------------------------------+
   |          +------------------+ |                     empty        +-----+
-> | (nse) -> | mkFuncDebuggable | | -> (computation, side-effect) -> |  f' |->
   |          +------------------+ |                                  +-----+
   +-------------------------------+
```

## Function Application

Being able to compose functions is fine, but many times we don't want to compose functions. Sometimes, it's just easier to apply functions one after another.

Here is an example of that with NO `side-effects`:

```hs	  
y :: Float
y = 12.345 & f & g
```
Here `12.345` is first applied to `f` and then its results are applied to `g`.

While this example is pretty simplistic, in real-world programming, things can get more complicated making this form of programming highly desirable.

The `side-effect` version of function appication is a bit more complex. We can write:

```hs
12.345 & f' :: (Float, String)
```

but we cannot pass the result directly into `g'` since `g'` has the signature `Float -> (Float, String)`.

Just like with composition, we need a special apply function for `Debuggables` that will take the `(Float, String)` and apply it to a function like `g' :: Float -> (Float, String)`:

```hs
applyDebuggable :: (a, String) -> (a -> (b, String)) -> (b, String)
applyDebuggable (x, l) f' = let (r, s) = f' x in (r, l ++ s)
```
Now with `applyDebuggable`, we can finally write `y'`:

```hs
y' :: (Float, String)
y' = (12.345 & f') `applyDebuggable` g'
```

This version of `y'` uses both `&` and `applyDebuggable`. To write a version that only uses `applyDebuggable` we must take the `Float` and turn it into `(Float, String)` or more generally:

```hs
mkDebuggable :: a -> (a, String)
mkDebuggable x = (x, "")
```

`mkDebuggable` takes any value and wraps it in a `Debuggable`. As an aside, we can rewrite `mkFuncDebuggable` in terms of `mkDebuggable`:

```hs
mkFuncDebuggable' :: (a -> b) -> (a -> (b, String))
mkFuncDebuggable' p x = mkDebuggable (p x)
```

Now that we have `mkDebuggable`, we can write `y'` using only `applyDebuggable`:

```hs
y' :: (Float, String)
y' = mkDebuggable 12.345 `applyDebuggable` f' `applyDebuggable` g'
```

We now have the ability to do function application to `Debuggable` functions.

In fact, we can now rewrite `compDebuggable` in terms of `applyDebuggable`:

```hs
compDebuggable :: (b -> Debuggable c) -> (a -> Debuggable b) -> (a -> Debuggable c)
compDebuggable g' f' x = f' x `applyDebuggable` g'
```

## Debuggable Type

We've been referring to the output of our `side-effect` functions as `Debuggable`, but now it's time to give it a proper type.

```hs
type Debuggable a = (a, String)
```

And here again are the functions that support this type, this time using our new type:

```hs
applyDebuggable :: Debuggable a -> (a -> Debuggable b) -> Debuggable b
applyDebuggable (x, l) f' = let (r, s) = f' x in (r, l ++ s)

mkDebuggable :: a -> Debuggable a
mkDebuggable x = (x, "")
```

(`mkFuncDebuggable` and `compDebuggable` have been left out since they can be written in terms of `mkDebuggable` and `applyDebuggable`, respectively)

So we are left with 3 very useful things that let us work easily and consistently with `Debuggables`:

1. Our special `side-effect` type (`Debuggable`)
2. A way to apply our special `side-effect` functions (`applyDebuggable`)
3. A way to turn non-special types into our special type (`mkDebuggable`)

Seems like we have everything we need to work with our `Debuggable` functions and their outputs easily and consistently.

## Time to step back and take inventory

#### What is `Debuggable`?*

In general terms, `Debuggable` is a `computation` **with** `side-effects`.

Functions that operate on `Debuggables` will operate on the `computation` and optionally produce a `side-effect`. The previous `computation` value is input and optionally modified by the function to produce a new `computation` value along with a `side-effect`.

The previous `side-effect` value is modified in a similar way EXCEPT it isn't modified by the function, but instead is modified by `applyDebuggable`.

So `side-effects` are "managed" by the code that applies them. This is a nice feature since we don't have to keep writing code to manage the `side-effects` over and over again.

\* *(Answer: A Monad)*

#### How can I generalize `#2` and `#3` in code?*

Instead of having to come up with names like `applyDebuggable` and `mkDebuggable` it would be nice to have a common name for these operations.

This is where `type classes` come in handy. We just have to figure out a good name for our `type class` functions. Fortunately, these already exist in Haskell, but unfortunately, the names are terrible especially for the newcomer.

\* *(Answer: type class functions `>>=` and `return`)*

## The dreaded M-word

Let's finally take what we've done with `Debuggable` and see how it relates to Monads.

#### `mkDebuggable`
First, take `mkDebuggable`. In the Monad type class, it's called `return` (see what I mean about terrible names). This name makes sense when you learn about `do` syntax in Haskell.

`mkDebuggable` took a non-Debuggable Value and made it a Debuggable Value.

`return` takes non-Monadic Value and makes it Monadic Value where the Monadic Value is a value in the Monad.

In the Debuggable Monad, a non-Monadic Value could be 12.345 and `return 12.345` is a Monadic Value of `(12.345, "")`.

#### `applyDebuggable`
Second, take `applyDebuggable`. In the Monad type class, it's called `bind` (another lousy name). It's so commonly used that it has its own operator `(>>=)`.

You can think of `bind` as binding the Monadic Value to a Monadic Function. This makes sense when you look at `applyDebuggable`'s signature:

```hs
applyDebuggable :: Debuggable a -> (a -> Debuggable b) -> Debuggable b
```
The Monadic Value inside `Debuggable a` is bound to the Monadic Function `a -> Debuggable b` to produce `Debuggable b`.

### Monad type class

Here is the Monad type class definition:

```hs
class Applicative m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
```

This is the minimal definition (see Prelude for full definition).

Notice how `m a` is analogous to `Debuggable a` and `a -> m b` is analogous to `a -> Debuggable b`. Basically, where we had `Debuggable` we now have `m`.

### Debuggable implementation

The following is the full implementation of `Debuggable` and its `Functor`, `Applicative` and `Monad` instances. `Functor` and `Applicative` are shown only for completeness. We are mainly concerned with `Monad`.

```hs
module Debuggable where

newtype Debuggable a = Debuggable (a, String)

instance Functor Debuggable where
  fmap f (Debuggable (x, s)) = Debuggable (f x, s)

instance Applicative Debuggable where
  pure x = Debuggable (x, "")
  Debuggable (f, s) <*> Debuggable (x, t) = Debuggable (f x, s ++ t)

instance Monad Debuggable where
  return = pure
  Debuggable (x, s) >>= f' = let Debuggable (r, t) = f' x in Debuggable (r, s ++ t)
```

In the Monad implementation, `return` leverages the `Applicative`'s `pure` function. Notice this implementation is equivalent to our `mkDebuggable`.

Bind, or more accurately, `>>=`, is implemented exactly like our `applyDebuggable`. **Notice how the `side-effects` are managed by the Monad's implmentation.**

Functions like `f'` and `g'` only have to worry about their own `side-effects`. The bind function concatenates the side-effects. (Note we could have used `<>` which is the `mconcat` operator from `Data.Monoid`, but I left it out for simplicity sake.)

## What about the functions we threw away

Remember `mkFuncDebuggable` and `compDebuggable`. Well, these are derivative functions, that is, they can be written in terms of other functions, viz. the Monad functions.

#### `lift`

`mkFuncDebuggable` is a function called `lift` with the following implementation:

```hs
lift :: Monad m => (a -> b) -> a -> m b
lift f x = return (f x)
```

N.B., `lift` is a highly overloaded function name in Haskell and there are many implementations for different scenarios. This implementation make the most sense for what we're doing here.

#### `<=<`

`compDebuggable` is the right-to-left version of `Kleisli composition`, `<=<`, which is composition of Monadic Functions, viz. functions of the form `Monad m => a -> m b`:

```hs
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(g' <=< f') x = f' x >>= g'
```

Notice how both `lift` and `<=<` are NOT part of a type class but instead leverage the implementation of `Monad m`. 

`lift` is written in terms of `return` and `<=<` is written in terms of `>>=`.

Therefore, these functions can work with any Monads, hence the `Monad m =>` portion of their signatures.

## Flow Control

Note that `>>=` can do **limited** flow control. In our implemenation of Debuggable, we only have 1 `type constructor` viz. `Debuggable`. But Monads such as `Maybe` and `Either` have 2.

`Maybe` has `Just a` and `Nothing` and `Either` has `Left a` and `Right b`.

Their bind implementations `short-circuit` function applications when `Maybe` encounters a `Nothing` or `Either` encounters a `Left a`.

Pictorially:

```
   +-----+      +-----+      +-----+
-> |  f' | +--> |  g' | +--> |  h' | +-->
   +-----+ |    +-----+ |    +-----+ |
           |            |            |
           +------------+------------+-> SHORT-CIRCUIT
```

In this picture, the bind may `short-circuit`, i.e. exit early, after `f'` or `g'` are executed.

The flow control is limited because the **only** control that is possible is an early exit.

It's **impossible** for the bind, for example, to skip `g'` and not `h'` because there is no guarantee that the types would line up.

The bind can only do 1 of 2 things.

1. Continue function application
2. Abort function application

## Conclusion

I'd recommend re-reading this and implementing these functions yourself just to get a good viceral feel for them.

I suspect that this, like all of the other Monad articles, will only provide a small view of the larger subject regarding Monads.

Hopefully, this has been a beginner friendly treatise of Monads.