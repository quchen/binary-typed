`binary-typed` changelog
========================

## 0.1

### 0.1.0.0

Initial release.

### 0.1.0.1

- Add testsuite to compare generated message lengths with vanilla `Binary` and
  the various different typed serializations.
- The .cabal file is hopefully fixed so that Hackage is able to build the docs
  automatically.



## 0.2

### 0.2.0.0

- Add `Hashed32` type representation with 32 bit, more suitable for short data
  than the previous `Hashed`, which was implicitly 64 bit long. `Hashed64` is
  also available in case the longer hash is needed.
- Type representations are now automatically cached by `encodeTyped`. If this
  data is long enough, it is even serialized in advance as well.

### 0.2.0.1

- Fix whitespace in docs to make the docs build properly on Hackage, which
  presumably has an older Haddock version

### 0.2.1.0

- Fix sharing not working at all (bad `encodeTyped` implementation)
- Pre-calculate certain functions in the benchmarks so they can be properly
  shared among invocations



## 0.3

### 0.3.0.0

- Add `Hashed5` type representation that has no size overhead compared to
  `Untyped`