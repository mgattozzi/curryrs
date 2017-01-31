# Testing Curryrs

Curryrs has two different libraries, one for Rust and one for Haskell.
These libraries allow interoperability between the two languages and
allows Rust code to be called in Haskell and Haskell in Rust. To account
for that two different types of tests are setup.

To run the Haskell in Rust test suite:

```bash
make test-rust
```

To run the Rust in Haskell test suite:

```bash
make test-haskell
```

To run the all of the test suites:

```bash
make test
```

To just build all of the test suites:

```bash
make test-build
```

# Adding Tests to Curryrs

## Haskell in Rust tests
If you want to add functions that are exported to Rust for testing you can
modify the code under `htest` so that you can export functions for use
in the `tests/haskell_import.rs` file. Import them in that file and add
it to the `ffi_test` function to make sure it works.

Note if you place it as a separate test the runtime might crash and it
would cause undesired behavior. If you can get this working without that
or these docs are out of date regarding that please send in a PR!

## Rust in Haskell tests
If you want to add functions that are exported to Haskell for testing you can
modify the code under `rtest` so that you can export functions for use
in the `haskell-test/Test.hs` file. Import them in that file and add
it to the `unitTests` function to make sure it works. The code uses the
Tasty framework so both `HUnit` and `Quickcheck` tests should work.
