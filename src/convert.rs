//! Methods to convert to or from FFI types that don't have an easy
//! one to one conversion

use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};

use crate::types::Boolean;

/// Extract bool from a Boolean
pub fn from_boolean(x: Boolean) -> Result<bool, ConversionError> {
  match x {
    0 => Ok(false),
    1 => Ok(true),
    _ => Err(ConversionError::Boolean),
  }
}

/// Possible errors when converting to Rust Types from FFI
/// or to FFI from Rust
#[derive(Debug, Copy, Clone)]
pub enum ConversionError {
  /// Thrown if the conversion is on a number that is not 0 or 1.
  /// Since 1 is being used as true and 0 as false any other number
  /// is considered invalid.
  Boolean,
}

/// Display implementation for ConversionErrors
impl Display for ConversionError {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    write!(f, "{}", self)
  }
}

/// Error implementation for ConversionErrors
impl Error for ConversionError {
  fn description(&self) -> &str {
    use crate::convert::ConversionError::*;
    match *self {
      Boolean => "Failed to convert to or from a Boolean",
    }
  }
}
