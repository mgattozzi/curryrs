build: hs rust

# Build curryrs Haskell Library
hs:
	cabal build

# Build curryrs Rust Library
rust:
	cargo build

# Run the tests in both libraries
test: _test-build
	cargo test
	cabal test

# Build the test libraries
_test-build: build
	cargo build --package rtest
	cd htest && cabal build

# Run the tests for Rust
test-rust: _test-build
	cargo test

test-haskell: _test-build
	cabal test

# Document both libraries
doc:
	cargo doc
	cabal haddock

# Clean everything up
clean:
	cargo clean
	cabal clean
	cd htest
	cabal clean
