# purescript-vec-dt

Dependently typed vectors in PureScript.

## Motivation

In code with I/O, it's often the case that we'll want to operate on two data structures only if they share certain properties. For example, we may want to operate on two lists _only_ if they are equal and otherwise raise an error.

This is usually done with code like this:

```purescript
prog = do
  list1 <- api0
  list2 <- api0
  when (length list1 /= length list2) $ throwError "err"
  pure $ zipWith (+) list1 list2
```

The issue with this code is that it relies making an assertion before every use of `zipWith`.

A better way to do this would be to reify the value-level assertion into the type level.

According to Wikipedia:

> In computer science and logic, a dependent type is a type whose definition depends on a value.

In the [tests](./test/Main.purs) of this library, you'll see the following commented-out code:

```purescript
test1 list0 list1 = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- vec list0
  v1 <- vec list1
  ipure $ zipWithE (+) v0 v1
```

If you comment it back in, you should see a red squiggly under `zipWithE`. That's because we have not made an assertion before zipWithE that the two lists are of equal length.

Instead, we need to do:

```purescript
test0 list0 list1 = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- vec list0
  v1 <- vec list1
  l /\ r <- assertEq (error "not eq") v0 v1
  ipure $ zipWithE (+) l r
```

Now, we have a typelevel assertion in our indexed monad that `v0` and `v1` are equal. Proof of this assertion lies in the values `l` and `r` and rides through the rest of the computation until `zipWithE`.

The assertion system also works for arbitrary append (`<+>`) and cons (`+>`) operations. In the following example, also from the tests, proof of equal length persists through the list append operation `<+>`.

```purescript
test6 list0 list1 list2 list3 = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- vec list0
  v1 <- vec list1
  v2 <- vec list2
  v3 <- vec list3
  l /\ r <- assertEq (error "not eq") v0 v1
  x /\ y <- assertEq (error "not eq") v2 v3
  ipure $ zipWithE (+) (l <+> y) (x <+> r)
```

This library brings dependent types to vectors by using the type system to allow certain operations, like `zipWithE`, to be performed only if certain assertions about values, like `assertEq`, succeed.

## TODO

- [ ] make equality transitive
- [ ] encode equality in a typeclass to avoid `assertEq`, `assertEq3` etc
