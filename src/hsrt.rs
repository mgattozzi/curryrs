//! Functions related to the Haskell Runtime wrapped up in a safe interface for
//! the user.
//!

use std::os::raw::{c_char, c_int};
use std::ptr;
use std::sync::{Once, ONCE_INIT};
use std::sync::atomic::{AtomicBool, ATOMIC_BOOL_INIT, Ordering};

extern {
	pub fn hs_init(argc: *mut c_int, argv: *mut *mut *mut c_char);
	pub fn hs_exit();
}

static START_ONCE: Once = ONCE_INIT;
static STOP_ONCE: Once = ONCE_INIT;
static STOPPED: AtomicBool = ATOMIC_BOOL_INIT;

/// Initialize the Haskell runtime. This function is safe to call more than once, and
/// will do nothing on subsequent calls.
///
/// The runtime will automatically be shutdown at program exit, or you can stop it
/// earlier with `stop`.
pub fn start() {
	START_ONCE.call_once(|| {
		start_impl();
		unsafe {
			::libc::atexit(stop_nopanic);
		}
	});
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

/// Stop the Haskell runtime before the program exits. This function may only be called
/// once during a program's execution.
///
/// It is safe, but not useful, to call this before the runtime has started.
///
/// # Panics
///
/// Will panic if called more than once.
pub fn stop() {
	if STOPPED.swap(true, Ordering::SeqCst) {
		panic!("curryrs: The GHC runtime may only be stopped once. See \
		        https://downloads.haskell.org/%7Eghc/latest/docs/html/users_guide\
		        /ffi-chap.html#id1 ");
	}
	stop_nopanic();
}

extern fn stop_nopanic() {
	STOP_ONCE.call_once(|| {
		unsafe { hs_exit() }; // does nothing if hs_init_count <= 0
	});
}
