-- |
-- Definitions of each FFI type that can be used in Rust. These are
-- standardized accross the Rust and Haskell Curryrs library for easy
-- translation of function headers between the two.
module Curryrs.Types (
    module Foreign.C.Types
  , module Foreign.C.String
  , Chr
  , Str
  , U8
  , U16
  , U32
  , U64
  , I8
  , I16
  , I32
  , I64
  , F32
  , F64
  , Boolean
  ) where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.C.String

-- We are only defining types that map to Rust types here
-- We don't need the full array of C types in Rust

-- |
-- Used to represent Char in both languages
type Chr = CChar

-- |
-- Used to represent Strings in both languages
type Str = CString

-- |
-- Used to represent 8 bit unsigned numbers in both languages
type U8  = Word8

-- |
-- Used to represent 16 bit unsigned numbers in both languages
type U16 = Word16

-- |
-- Used to represent 32 bit unsigned numbers in both languages
type U32 = Word32

-- |
-- Used to represent 64 bit unsigned numbers in both languages
type U64 = Word64

-- |
-- Used to represent 8 bit signed numbers in both languages
type I8  = Int8

-- |
-- Used to represent 16 bit signed numbers in both languages
type I16 = Int16

-- |
-- Used to represent 32 bit signed numbers in both languages
type I32 = Int32

-- |
-- Used to represent 64 bit signed numbers in both languages
type I64 = Int64

-- |
-- Used to represent 32 bit floating point numbers in both languages
type F32 = CFloat

-- |
-- Used to represent 64 bit floating point numbers in both languages
type F64 = CDouble

-- |
-- Used to represent Booleans in both languages
type Boolean = Word8
