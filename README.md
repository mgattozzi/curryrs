# Curryrs

Curryrs (a play on the name of Haskell Curry, rs for Rust libraries, and
it's pronunciation couriers) is a library for providing easy to use bindings
between Rust and Haskell Code. Given the type safety inherent in both
languages Curryrs seeks to bridge the gap between the two languages by
providing an interface between the two that makes writing FFI code
a relatively painless experience.

## Installation

In your Rust project in Cargo.toml:

```toml
[dependencies]
curryrs = "^0.1.0"
```

In your Haskell project in it's cabal file:

```cabal
build-depends: curryrs >= 0.1.0 < 0.2.0
```

## How to use Curryrs
Each library contains a module for the FFI types and one for conversion
to and from types that need extra work to do so. Right now this
conversion module only affects the Boolean type, however work in the
future of this module will likely include structs and other more complicated
data structures.

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

Theoretically if you need to export to Rust from Haskell:

```haskell
import Curryrs.Types

triple :: I64 -> I64
triple x = 3 * x

foreign export ccall triple :: I64 -> I64
```

Then in your Rust code:

```rust
use curryrs::types::*;

#[link(name = "your_library_name")]
extern {
    fn triple(x: I64) -> I64;
}

fn main() {
    let x = unsafe { triple(100) };
    // Tripled value is: 300
    println!("Tripled value is: {}", x);
}
```

However, at this moment I'm working on actually confirming this is the
case as there isn't much documentation on this yet and I'm still working
on setting up a proper Rust test suite.

In both cases the types are the same and so it makes it easy to work
with both without needing to worry about which C type to use and how to
get it to work, or which modules need to be imported. The C modules are
imported for both languages so the user doesn't need to worry about it.

## Contributing
See [CONTRIBUTING.md](CONTRIBUTING.md) for more information.

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
