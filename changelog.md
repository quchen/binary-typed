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

### 0.2.0.2

- Pre-calculate certain functions in the benchmarks so they can be properly
  shared among invocations
- Remove incorrect remark (from an earlier version) in the readme/.cabal about
  encoding being almost identical in performance to direct `Binary`