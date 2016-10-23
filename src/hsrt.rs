//! Functions related to the Haskell Runtime wrapped up in a safe interface for
//! the user.
//!

use std::os::raw::{c_char, c_int};
use std::ptr;

extern {
	pub fn hs_init(argc: *mut c_int, argv: *mut *mut *mut c_char);
	pub fn hs_exit();
}

/// Initialize the Haskell runtime
///
/// func is what you pass to the argv part of the C Call to hs_init
///
/// If you call `hs_start()` you absolutely must call `hs_stop()` forgetting to
/// do so will cause undefined behavior and the runtime will run without
/// stopping. This could cause memory leaks or a whole bunch of other
/// errors that can't be checked.
pub fn hs_start(mut func: String) {
	func.push('\0');
	let argv0 = func.as_ptr();
	let mut argv = [argv0 as *mut c_char, ptr::null_mut()];
	let mut argc = (argv.len() - 1) as c_int;
	unsafe {
		hs_init(&mut argc, &mut argv.as_mut_ptr());
	}
}

/// Stop the haskell runtime
///
/// This should only be called after the `hs_start()` function has been invoked

pub fn hs_stop() {
	unsafe{hs_exit()};
}
