extern crate curryrs;
use curryrs::types::*;
use curryrs::hsrt::*;

#[link(name = "htest", kind = "dylib")]
extern {
	pub fn triple(x: I32) -> I32;
}


fn triple_num(x: I32) -> I32 {
		hs_start("triple".to_string());
		let y = unsafe{triple(x)};
		hs_stop();
		y
}

#[test]
fn triple_test() {
	assert_eq!(900, triple_num(300));
}
