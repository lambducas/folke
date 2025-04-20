module NativeFileDialog (
  doSomeLog,
  openDialogAndLogResult,
  openFolderDialog,
  openSaveDialog,
  openDialog
) where

import Foreign.C.Types
import Foreign.C (peekCString, CString, newCString)
import Foreign.Ptr

foreign import ccall unsafe "printFakeErrorMessage" printFakeErrorMessage :: IO CInt
foreign import ccall unsafe "openDialogAndLogResult" openDialogAndLogResult :: IO CInt
foreign import ccall unsafe "pickFolder" pickFolderC :: IO (Ptr CChar)
foreign import ccall unsafe "saveDialog" saveDialogC :: Ptr CChar -> IO (Ptr CChar)
foreign import ccall unsafe "openDialog" openDialogC :: IO (Ptr CChar)

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

openSaveDialog :: String -> IO (Maybe String)
openSaveDialog filter = do
  cFilter <- newCString filter
  str <- saveDialogC cFilter
  path <- peekCString str

  if path == ""
    then return Nothing
    else return (Just path)

openDialog = do
  str <- openDialogC
  path <- peekCString str

  if path == ""
    then return Nothing
    else return (Just path)