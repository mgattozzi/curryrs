//! Contains Type Equivalency for Haskell Types

pub use std::os::raw::{
	c_char,
	c_float,
	c_double
};

/// FFI Version of bool
pub type Boolean = u8;

/// FFI Version of char
pub type Chr = c_char;

/// FFI version of str
pub type Str = *const c_char;

/// FFI version of u8
pub type U8  = u8;

/// FFI version of u16
pub type U16 = u16;

/// FFI version of u32
pub type U32 = u32;

/// FFI version of u64
pub type U64 = u64;

/// FFI version of i8
pub type I8  = i8;

/// FFI version of i16
pub type I16 = i16;

/// FFI version of i32
pub type I32 = i32;

/// FFI version of i64
pub type I64 = i64;

/// FFI version of f32
pub type F32 = c_float;

/// FFI version of f64
pub type F64 = c_double;
