module CreateMenu where

import Foreign.C.Types
import Foreign.C (peekCString, CString, newCString)
import Foreign.Ptr
import Data.Maybe (fromMaybe)

foreign import ccall unsafe "CreateMenu" createMenuC :: IO CInt

createMenu = do
  _ <- createMenuC

  return ()