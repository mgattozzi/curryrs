//! Macros used to make binding code easier

#[macro_export]
/// All functions with this will have an unsafe import in Haskell
macro_rules! unsafe_ffi {
	($(fn $i:ident($($arg:ident: $argty:ty),*) -> $ret:ty {
		$($body:stmt);*
	})*) => ($(
		#[no_mangle]
		pub extern fn $i($($arg: $argty),*) -> $ret {
			$($body);*
		}
	)*)
}

#[macro_export]
/// All functions with this will have a safe import in Haskell
macro_rules! safe_ffi {
	($(fn $i:ident($($arg:ident: $argty:ty),*) -> $ret:ty {
		$($body:stmt);*
	})*) => ($(
		#[no_mangle]
		pub extern fn $i($($arg: $argty),*) -> $ret {
			$($body);*
		}
	)*)
}
