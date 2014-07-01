[![(Travis image broken)](https://travis-ci.org/quchen/binary-typed.png?branch=master)][travis]

# Typed Binary lib

Standard `Binary` serializes to `ByteString`, which is an untyped format;
deserialization of unexpected input usually results in unusable data.

This module is built around a `Typed` type, which allows serializing both a
value and the type of that value; deserialization can then check whether the
received data was sent assuming the right type, and error messages may provide
insight into the type mismatch.

More information can be found on the [binary-typed Hackage page][hackage], or
you can generate the documentation yourself (via `cabal haddock`).


[travis]: https://travis-ci.org/quchen/binary-typed
[hackage]: http://hackage.haskell.org/package/binary-typed