{-# LANGUAGE ForeignFunctionInterface #-}

module CreateMenu where

import Foreign.C.Types
import Foreign.C (peekCString, CString, newCString)
import Foreign.Ptr
import Data.Maybe (fromMaybe)

foreign import ccall unsafe "CreateMenu" createMenuC :: IO CInt

createMenu = do
  _ <- createMenuC

  return ()

fibonacci :: Int -> Int
fibonacci n = fibs !! n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacci_hs :: CInt -> CInt
fibonacci_hs = fromIntegral . fibonacci . fromIntegral

foreign export ccall fibonacci_hs :: CInt -> CInt