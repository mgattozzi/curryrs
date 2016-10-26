{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import Curryrs.Types

triple :: I64 -> I64
triple x = 3 * x

foreign export ccall triple :: I64 -> I64
