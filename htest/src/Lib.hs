{-# LANGUAGE ForeignFunctionInterface #-}
module Lib where
import System.Environment (getProgName)
import Data.Int
import Foreign.C.String
import Foreign.C.Types

type I64 = Int64
type Str = CString

triple :: I64 -> I64
triple x = 3 * x

foreign export ccall triple :: I64 -> I64

getProgNameStr :: IO Str
getProgNameStr = do
  putStrLn "Testing one two three"
  getProgName >>= newCString

foreign export ccall getProgNameStr :: IO Str
