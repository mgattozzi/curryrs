HTEST:=$(realpath htest)

build: hs cargo

# Build curryrs Haskell Library
hs:
	@  (command -v stack 1,2>/dev/null && stack build) \
	|| (command -v cabal 1,2>/dev/null && cabal build) \
	|| (echo "ERROR: cabal or stack not found" && exit 1)
	@echo "Haskell Library built"

# Build curryrs Rust Library
cargo:
	@[ -x ${RUSTC} ] || (echo "ERROR: rust compiler (rustc) not found" && exit 1)
	@cargo build --release
	@echo "Rust Library built"

# Run the tests in both libraries
test: test-build
	@cargo test
	@  (command -v stack 1,2>/dev/null && stack test) \
	|| (command -v cabal 1,2>/dev/null && cabal test) \
	|| (echo "ERROR: cabal or stack not found" && exit 1)

# Build the test libraries
test-build: build
	@(cd rtest && cargo build --release)
	@  (command -v stack 1,2>/dev/null && cd htest && stack build) \
	|| (command -v cabal 1,2>/dev/null && cd htest && cabal build) \
	|| (echo "ERROR: cabal or stack not found" && exit 1)

# Run the tests for Rust
test-rust: test-build
	@cargo test

test-haskell: test-build
	@  (command -v stack 1,2>/dev/null && stack test) \
	|| (command -v cabal 1,2>/dev/null && cabal test) \
	|| (echo "ERROR: cabal or stack not found" && exit 1)

# Document both libraries
doc:
	@cargo doc
	@  (command -v stack 1,2>/dev/null && stack haddock) \
	|| (command -v cabal 1,2>/dev/null && cabal haddock) \
	|| (echo "ERROR: cabal or stack not found" && exit 1)

# Clean everything up
clean:
	@cargo clean
	@  (command -v stack 1,2>/dev/null && stack clean) \
	|| (command -v cabal 1,2>/dev/null && cabal clean) \
	|| (echo "ERROR: cabal or stack not found" && exit 1)
	@  (command -v stack 1,2>/dev/null && cd htest && stack clean) \
	|| (command -v cabal 1,2>/dev/null && cd htest && cabal clean) \
	|| (echo "ERROR: cabal or stack not found" && exit 1)
	@rm -rf *~ *.hi *.o *.so target/ G* *.a
	@(cd htest && rm -rf *~ *.hi *.o *.so target/ G* *.a)
	@(cd rtest && rm -rf *~ *.hi *.o *.so target/ G* *.a)
