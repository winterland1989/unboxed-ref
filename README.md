unboxed-ref
===========

[![Hackage](https://img.shields.io/hackage/v/unboxed-ref.svg?style=flat)](http://hackage.haskell.org/package/unboxed-ref)
[![Test Status](https://img.shields.io/travis/winterland1989/unboxed-ref.svg)](https://travis-ci.org/winterland1989/unboxed-ref)

This package provide fast unboxed references for `ST` and `IO` monad and atomic operations for `IORefU Int` type. Unboxed reference is implemented using single cell `MutableByteArray s` to eliminate indirection overhead which `MutVar# s a` carry, on the otherhand unboxed reference only support limited type(instances of `Prim` class).

A simple diagram could show the difference between `IORef Int` with `IORefU Int`:

```
data Foo = Foo {-# UNPACK #-} (IORef Int)

        +-----------+    +-------------+    +---------+
        | Foo |  *  +--->+ MutVar# | * +--->+ I# | i# |
        +-----------+    +-------------+    +---------+

data Bar = Bar {-# UNPACK #-} (IORefU Int)

        +-----------+    +------------------------+
        | Bar |  *  +--->+ MutableByteArray# | i# |
        +-----------+    +------------------------+
```

Benchmark
---------

Modified from [this benchmark](https://marcotmarcot.wordpress.com/2010/03/13/performance-of-ioref/).

```bash
$ cd bench && cabal build
$ time ./dist/build/bench-ref/bench-ref
143
./dist/build/bench-ref/bench-ref  19.76s user 0.02s system 99% cpu 19.785 total
------------------------------------------------------------
$ time ./dist/build/bench-unboxed-ref/bench-unboxed-ref
143
./dist/build/bench-unboxed-ref/bench-unboxed-ref  16.66s user 0.02s system 99% cpu 16.694 total
------------------------------------------------------------
$ ./dist/build/bench-unboxed-ref-atomic/bench-unboxed-ref-atomic
50500000
------------------------------------------------------------
$ ./dist/build/bench-ref-atomic/bench-ref-atomic
3597361
```
