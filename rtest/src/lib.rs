#[macro_use]
extern crate curryrs;

use curryrs::types::*;

// Place each function you want exported into the safe_ffi! macro and it will
// export each one and place the pub extern for you!
safe_ffi! (

	fn double_input(x: I64) -> I64 {
		2 * x
	}

	fn square_input(x: U64) -> U64 {
		x * x
	}

	fn triple_input(x: I64) -> I64 {
		x * x * x
	}

	fn get_true() -> Boolean {
		1
	}

	fn get_false() -> Boolean {
		0
	}
);
