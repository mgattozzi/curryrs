extern crate curryrs;
use curryrs::types::*;

#[test]
fn triple_test() {
	#[link(name = "chtest", kind="dylib")]
	#[link(name = "htest", kind="dylib")]
	extern {
		pub fn tripleNum(x:I32) -> I32;
	}

	assert_eq!(900, unsafe{tripleNum(300)});
}
