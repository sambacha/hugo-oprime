+++
title = "Covariant and Contravariant Functors"
date = "2021-08-24"
menu = "main"
+++

## Covariant and Contravariant Functors

So we create a new function, `h`, with a type we can convert, i.e. `b -> Int` and we have `f`, our conversion function convert it. Then we apply that to our original function, `g`, giving us:

```haskell
instance Functor F5 where
  fmap f (F5 g) = F5 (\h -> g (h . f))
```

Looking at `h` in detail and replacing types and reducing gives us:

```
\h             ->   g                   (   h          .    f        )
(b -> Int)     ->   (a -> Int) -> Int   (   b -> Int   .    a -> b   )
(b -> Int)     ->   (a -> Int) -> Int   (          a -> Int          )
(b -> Int)     ->                      Int
```

This is exactly what we were aiming for, i.e. `(b -> Int) -> Int`.

## Mapping from `(Int -> a) -> Int` to `(Int -> b) -> Int` (#6)


So far, we have:

```haskell
h :: (a -> Int)
f :: (b -> a)

h . f :: b -> Int
hh :: (b -> Int) -> Int

\h -> hh(h . f) :: (a -> Int) -> Int
```

`\h -> hh(h . f)` is perfect for applying to our original function `g`, i.e. `((a -> Int) -> Int) -> Int` giving us our final solution:

```haskell
newtype F7 a = F7 (((a -> Int) -> Int) -> Int)

instance Contravariant F7 where
  contramap f (F7 g) = F7 (\hh -> g (\h -> hh(h . f)))
```
Looking at the final function in detail:

```haskell
\hh                 -> g                            (\h         -> hh                 (h          . f       ))
((b -> Int) -> Int) -> (((a -> Int) -> Int) -> Int) ((a -> Int) -> (b -> Int) -> Int) ((a -> Int) . (b -> a)))
((b -> Int) -> Int) -> (((a -> Int) -> Int) -> Int) ((a -> Int) -> (b -> Int) -> Int) (        b -> Int     ))
((b -> Int) -> Int) -> (((a -> Int) -> Int) -> Int) ((a -> Int) ->                Int                        )
((b -> Int) -> Int) -> (((a -> Int) -> Int) -> Int) (                 (a -> Int) -> Int                      )
((b -> Int) -> Int) ->                                       Int
```
Looks like we can keep `Generalization #2` for now. Maybe with enough experience we can further generalize it.

## Mapping from `((Int -> a) -> Int) -> Int` to `((Int -> b) -> Int) -> Int` (#8)

Looking at the pattern of solutions for #5 and #6, we can see that there are 2 difference:

1. one is `Functor` and the other is `Contravariant`
2. one uses `h . f` and the other uses `f . h`

Note that we make no assumption that there is any correlation between 1 and 2 here, i.e. `Functor` doesn't necessarily always use `h . f`. We just want to pick the opposite of whichever one #7 uses to create #8.

So let's take a stab at this mapping by just applying these 2 inversions to solution for #7, viz.:

1. `Contravariant` in #7 becomes `Functor`
2. `h . f` in #7 becomes `f . h`

```haskell
newtype F8 a = F8 (((Int -> a) -> Int) -> Int)

instance Functor F8 where
  fmap f (F8 g) = F8 (\hh -> g (\h -> hh(f . h)))
```

Turns out that that approach worked great.
