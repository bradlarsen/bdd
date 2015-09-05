# bdd
Binary decision diagrams in C, with an emphasis on correctness,
clarity, and performance

This library also includes Haskell bindings in the `hs-bdd`
directory.  In fact, the C library is built as part of building the
Haskell bindings, and is not currently set up to build in isolation.

The `hs-bdd-test` directory contains a Haskell project to build test
and example programs for the library.

The `c-templates` directory contains a bespoke mechanism for
instantiating "template" C code.  This is used to generate type-safe
collection data structures in C that are used within the BDD
library.  (If I were doing this again, I'd use C++ from the outset
in the BDD library code, but expose a C API.)

# Future Work

- Improve the test coverage
- Add support for automated testing using Travis CI
- Merge the `hs-bdd-test` programs with the `hs-bdd` directory and
  use Cabal's now built-in support for tests
- Switch to C++ as the implementation language so we can eliminate
  the bespoke C template mechanism
- Implement variable reordering algorithms --- there is already an
  implementation of the `swap adjacent variables in-place`
  primitive, but there aren't any implementations of higher-level
  reorderings
