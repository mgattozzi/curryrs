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

#[test]
fn ffi_test() {
	hsrt::start();
	// TODO Split these tests up
	let y = unsafe { triple(300) };
	let prog_name = unsafe { getProgNameStr() };
	hsrt::stop();
	assert_eq!(900, y);
	assert!(!prog_name.is_null());
	let prog_name_str = unsafe { CStr::from_ptr(prog_name) }.to_str().unwrap();
	let argv0 = env::args().nth(0).unwrap();
	let argv0_file_name = Path::new(&argv0).file_name().unwrap();
	assert_eq!(prog_name_str, argv0_file_name.to_str().unwrap());
}
