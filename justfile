build: hs rust

# Build curryrs Haskell Library
hs:
	cd haskell && cabal build

# Build curryrs Rust Library
rust:
	cd rust && cargo build

# Run the tests in both libraries
test: test-rust test-haskell

# Build the test libraries
_test-build: build _htest _rtest

_htest:
	cd haskell/htest && cabal build

_rtest:
	cd rust && cargo build --package rtest

# Run the tests for Rust
test-rust: _test-build
	cd rust && cargo test

test-haskell: _test-build
	cd haskell && cabal test

# Document both libraries
doc: doc-rust doc-haskell

doc-rust:
	cd rust && cargo doc

doc-haskell:
	cd haskell && cabal haddock

# Clean everything up
clean:
	cd rust && cargo clean
	cd haskell && cabal clean
	cd haskell/htest && cabal clean
