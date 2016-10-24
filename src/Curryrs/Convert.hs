-- |
-- The Convert module contains various functions used for converting
-- values to or from their FFI form. It also contains Error types in
-- case conversions to not go as planned.
module Curryrs.Convert (
    fromBoolean
  , ConversionError
  ) where

import Curryrs.Types (Boolean)

-- |
-- This method tries to to turn a number returned
-- by FFI into a Bool. It's wrapped in an either
-- in case the underlying number representing the Boolean
-- is not 0 or 1
--
-- >>> fromBoolean 1
-- Right True
--
-- >>> fromBoolean 0
-- Right False
--
-- >>> fromBoolean 3
-- Left Failed to extract a boolean value in the use of fromBoolean. Number was not 0 or 1
fromBoolean :: Boolean -> Either ConversionError Bool
fromBoolean x = case x of
  0 -> (Right False)
  1 -> (Right True)
  _ -> (Left Boolean)

-- |
-- Error data type for conversions that have failed going to or from
-- their FFI form
data ConversionError = Boolean
  deriving Eq

instance Show ConversionError where
  show Boolean = "Failed to extract a boolean value in the use of fromBoolean. Number was not 0 or 1"
