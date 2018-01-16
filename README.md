# Curryrs

Curryrs (a play on the name of Haskell Curry, rs for Rust libraries, and
it's pronunciation couriers) is a library for providing easy to use bindings
between Rust and Haskell Code. Given the type safety inherent in both
languages Curryrs seeks to bridge the gap between the two languages by
providing an interface between the two that makes writing FFI code
a relatively painless experience.

This library has only been tested with GHC 8.0.1 and Rust Stable.
To run the test suite you'll need gcc.

## Project Status

This library used to not work due to some linking issues. It's not working still
right now in it's current state but a fix is on the way see [here](https://github.com/mgattozzi/curryrs/issues/20)
for more details!

## Installation

In your Rust project in Cargo.toml:

```toml
[dependencies]
curryrs = "^0.2.0"
```

In your Haskell project in it's cabal file:

```cabal
build-depends: curryrs >= 0.2.0 < 0.3.0
```

## How to use Curryrs
Each library contains a module for the FFI types and one for conversion
to and from types that need extra work to do so. Right now this
conversion module only affects the Boolean type, however work in the
future of this module will likely include structs and other more complicated
data structures.

### Rust in Haskell
If you want to create functions that export to Haskell from Rust do the
following:

```rust
#[macro_use]
extern crate curryrs;

use curryrs::types::*;

// Place each function you want exported into the safe_ffi! macro and it will
// export each one and place the pub extern for you!
safe_ffi! (

	fn double(x: I32) -> I32 {
		2 * x
	}

	fn square(x: U64) -> U64 {
		x * x
	}

	fn cube(x: I64) -> I64 {
		x * x * x
	}

);
```

Currently this macro doesn't work if unsafe is put in as part of the
`fn` header. There are two macros: `safe_ffi!` and `unsafe_ffi!`. While
they are both the same for now when a binary is created to help
auto generate the bindings it will create unsafe or safe imports to
Haskell depending on which macros the functions are in. The recommended
use case is `safe_ffi!` for most of what you'll need.

Then in your Haskell program:

```haskell
import Curryrs.Types

foreign import ccall "double" double :: I64 -> I64
foreign import ccall "square" square :: I64 -> I64
foreign import ccall "cube" cube :: I64 -> I64

quadruple :: I64 -> I64
quadruple x = double $ double x

fourthPower :: I64 -> I64
fourthPower x = square $ square x

ninthPower :: I64 -> I64
ninthPower x = cube $ cube x
```

### Haskell in Rust
To run your Haskell code in Rust do the following steps:

First write and export the code you want for Haskell and use
the Curryrs.Types module to have FFI compatible types.

```haskell
import Curryrs.Types

foreign export ccall fourth :: I64 -> I64
foreign export ccall fifth :: I64 -> I64
foreign export ccall sixth :: I64 -> I64

fourth :: I64 -> I64
fourth x = x * x * x * x

fifth :: I64 -> I64
fithh x = x * x * x * x * x

sixth :: I64 -> I64
sixth x = x * x * x * x * x * x
```

In your cabal file add the following lines:

```cabal
other-extensions: ForeignFunctionInterface

-- It should end with .so if you're on Linux, .dylib for Mac, and
-- .dll for Windows
ghc-options: -dynamic -fPIC -shared -o lib{your_library_name_here}.so
```

Now in your Cargo.toml file add the following under package:

```toml
build = "build.rs"
```

Then in your `build.rs` file:

```rust
fn main() {
  println!("cargo:rustc-link-search=native={path_to_your_haskell_library_directory}");
  println!("cargo:rustc-link-lib=native={library_name_w/o_lib_and_extension}");
}
```

This links your Haskell library in at compilation. Now for the actual
code itself:

```rust
extern crate curryrs;
use curryrs::hsrt::{start,stop};
use curryrs::types::I64;

extern {
  pub fn fourth(x: I64) -> I64;
  pub fn fifth(x: I64) -> I64;
  pub fn sixth(x: I64) -> I64;
}

fn main() {
  // Input is whatever you want to pass to argv whenever
  // you start the Haskell Runtime. You need to start it
  // or calls to Haskell code will fail.
  start("Haskell Functions".to_string());

  println!("2^4 is: {}", unsafe{fourth(2)});
  println!("2^5 is: {}", unsafe{fifth(2)});
  println!("2^6 is: {}", unsafe{sixth(2)});

  // You need to make sure the runtime is stopped
  // otherwise you'll have undefined behavior
  // and wasted resources.
  stop();
}
```

This makes it easy to do without needing to muck around with linking the
right libraries and you're easily able to call the runtime you want.

The library also allows you to choose which version of the Haskell
Runtime you want to use. By default it uses the non-threaded version.
You can choose which one you want with a feature flag in `Cargo.toml`

```toml
[dependencies]
# If you need the threaded runtime put this:
curryrs = { version = "^0.2.0", features = "threaded" }

# If you need the threaded runtime w/ logging put this:
curryrs = { version = "^0.2.0", features = "threaded_l" }

# If you need the threaded runtime w/ debug output put this:
curryrs = { version = "^0.2.0", features = "threaded_debug" }
```

## Bug Reports
If you encounter errors of any sort please take a look in the issue
tracker first. If your error is already there or has been closed before
take a look at how it was solved or contribute to the open bug by
explaining what has happened while using the library. Duplicates will be
marked and closed.

## Contributing
See [CONTRIBUTING.md](docs/CONTRIBUTING.md) for more information.

## Tests
See [TESTS.md](docs/TESTS.md) for more information.

## Changelog
To see a list of changes between version take a look at [CHANGELOG.md](docs/CHANGELOG.md)
for more information.

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
