extern crate curryrs;

use std::env;
use std::ffi::CStr;
use std::path::Path;

use curryrs::types::*;
use curryrs::hsrt;

#[link(name = "htest", kind = "dylib")]
extern {
	pub fn triple(x: I32) -> I32;
	pub fn getProgNameStr() -> Str;
}

fn triple_num(x: I32) -> I32 {
	hsrt::start();
	unsafe { triple(x) }
}

#[test]
fn ffi_test() {
	// TODO Use the threaded Haskell runtime to let tests run safely in
	//      parallel, allowing separate test functions.

	assert_eq!(900, triple_num(300));

	hsrt::start();
	let prog_name = unsafe { getProgNameStr() };
	assert!(!prog_name.is_null());
	let prog_name_str = unsafe { CStr::from_ptr(prog_name) }.to_str().unwrap();
	let argv0 = env::args().nth(0).unwrap();
	let argv0_file_name = Path::new(&argv0).file_name().unwrap();
	assert_eq!(prog_name_str, argv0_file_name.to_str().unwrap());
}
