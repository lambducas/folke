module NativeFileDialog (
  doSomeLog,
  openDialogAndLogResult,
  openFolderDialog,
  openSaveDialog
) where

import Foreign.C.Types
import Foreign.C (peekCString, CString)
import Foreign.Ptr

foreign import ccall unsafe "printFakeErrorMessage" printFakeErrorMessage :: IO CInt
foreign import ccall unsafe "openDialogAndLogResult" openDialogAndLogResult :: IO CInt
foreign import ccall unsafe "pickFolder" pickFolderC :: IO (Ptr CChar)
foreign import ccall unsafe "saveDialog" saveDialogC :: IO (Ptr CChar)

doSomeLog = do
  putStrLn "BRURHEHHADHDBD BDDd :)"

    -- My fake error
  _ <- printFakeErrorMessage

  return ()

openFolderDialog = do
  str <- pickFolderC
  path <- peekCString str

  if path == ""
    then return Nothing
    else return (Just path)

openSaveDialog = do
  str <- saveDialogC
  path <- peekCString str

  if path == ""
    then return Nothing
    else return (Just path)