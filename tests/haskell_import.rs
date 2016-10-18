extern crate curryrs;
use curryrs::types::*;
use std::os::raw::c_int;
use std::ptr;

#[link(name = "htest", kind = "dylib")]
extern {
	pub fn triple(x: I32) -> I32;
}

include!(concat!(env!("OUT_DIR"), "/hs_rts.rs"));

extern {
	pub fn hs_init(argc: *mut c_int, argv: *mut *mut *mut c_char);
	pub fn hs_exit();
}

fn triple_num(x: I32) -> I32 {
	let mut argv0 = *b"triple_test\0";
	let mut argv = [argv0.as_mut_ptr() as *mut c_char, ptr::null_mut()];
	let mut argc = (argv.len() - 1) as c_int;

	unsafe {
		hs_init(&mut argc, &mut argv.as_mut_ptr());
		let y = triple(x);
		hs_exit();
		y
	}
}

#[test]
fn triple_test() {
	assert_eq!(900, triple_num(300));
}
