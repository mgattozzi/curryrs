build: hs cargo

hs:
	@(cd hs2rs && stack build && ghc -shared -o libinter.so inter.c libhs.so -fPIC)

cargo:
	@cargo build

run: hs
	@cargo run

test: build
	@cargo test

doc:
	@cargo doc
	@(cd hs2rs && stack haddock)

clean:
	@cargo clean
	@(cd hs2rs && stack clean && rm *.so)
