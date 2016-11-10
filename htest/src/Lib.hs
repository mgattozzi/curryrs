{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import System.Environment (getProgName)
import Foreign.C.String (newCString)

import Curryrs.Types

triple :: I64 -> I64
triple x = 3 * x

foreign export ccall triple :: I64 -> I64

getProgNameStr :: IO Str
getProgNameStr = getProgName >>= newCString

foreign export ccall getProgNameStr :: IO Str
