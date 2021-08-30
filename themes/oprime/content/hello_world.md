---
title: " Is composition just a Poor Man's Monad?"
date: 2017-10-17T14:00:45Z
draft: false
weight: 11
---

 # Is composition just a Poor Man's Monad?

Take the following composition:

```hs
f . g . h
```

The order of execution is `h`, `g` and finally `f`. There is no way for `f` to execute before `g` and it's not possible for `g` to execute before `h` because each function is "waiting" for the output of the previous.

If you only think of Monads as sequential computers, i.e. a Monad is something that runs it's computation sequentially, then Functional Composition is sufficient to accomplish that.

But, items 2 and 3 are only supported by Monads.

## There's more to Monads than sequential computation

Let's look at the other features that Monads can provide.

### Flow Control (short-circuit)

Short-circuiting is a common feature when failures happen in a Monadic computation.

`Maybe` is an example of a Monad that short-circuits the computation as soon as one of the operations fails, i.e. returns Nothing.

Below is the implementation for `Functor`, `Applicative` and `Monad` for Maybe. Notice how `(>>=)` doesn't call the function when the previous computation fails with a `Nothing`.

```hs
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  Just ff <*> x = fmap ff x

instance Monad Maybe where
  Nothing >>= _ = Nothing 		-- short-circuit happens here
  Just xx >>= f = f xx
```

Here's an example of how a short-cicuit would occur in some code. Assume that `tryToGetX` succeeds, i.e. returns `Just x` and `alwaysFails` fails and returns `Nothing`.

```hs
m = do
  x <- tryToGetX
  y <- alwaysFail x
  tryToGetZ y
```

Since `alwaysFails` returns `Nothing`, `tryToGetZ` will never be called and the value `m` will be `Nothing`.

It's easier to see this when you don't use the `do` syntax. The following is the equivalent of the `do` block above

```hs
m = tryToGetX >>= (\x -> alwaysFail x >>= (\y -> tryToGetZ y)) 
```

At any point, `>>=` can decide to NOT call the function, which means that a Monad can short-circuit the computation at its discretion.

In this case, the second `>>=` doesn't call `(\y -> tryToGetZ y)` since the left side, `x -> alwaysFail x`, evaluates to `Nothing`.
