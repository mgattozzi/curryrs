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
/// If you call `hs_start()` you absolutely must call `hs_stop()` forgetting to
/// do so will cause undefined behavior and the runtime will run without
/// stopping. This could cause memory leaks or a whole bunch of other
/// errors that can't be checked.
pub fn start() {
	start_impl();
}

#[cfg(not(windows))]
fn start_impl() {
	// OsString is expected to contain either byte-sized characters or UTF-8
	// on every platform except Windows.
	//
	// It's safe to unwrap the CString here as program arguments can't
	// contain nul bytes.
	use std::ffi::CString;
	use std::os::unix::ffi::OsStrExt;
	let mut args: Vec<_> = ::std::env::args_os()
		.map(|s| CString::new(s.as_os_str().as_bytes()).unwrap().into_bytes_with_nul())
		.collect();
	let mut argv = Vec::with_capacity(args.len() + 1);
	for ref mut arg in &mut args {
		argv.push(arg.as_mut_ptr() as *mut c_char);
	}
	argv.push(ptr::null_mut());
	let mut argc = args.len() as c_int;
	unsafe {
		hs_init(&mut argc, &mut argv.as_mut_ptr());
	}
}

#[cfg(windows)]
fn start_impl() {
	// GHC on Windows ignores hs_init arguments and uses GetCommandLineW instead.
	// See https://hackage.haskell.org/package/base-4.9.0.0/docs/src/GHC.Environment.html
	let mut argv0 = *b"\0";
	let mut argv = [argv0.as_mut_ptr() as *mut c_char, ptr::null_mut()];
	let mut argc = 1;
	unsafe {
		hs_init(&mut argc, &mut argv.as_mut_ptr());
	}
}

/// Stop the haskell runtime
///
/// This should only be called after the `hs_start()` function has been invoked

pub fn stop() {
	unsafe{hs_exit()};
}
