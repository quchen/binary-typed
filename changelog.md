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