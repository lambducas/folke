module NativeFileDialog (
  doSomeLog,
  openDialogAndLogResult
) where

import Foreign.C.Types

foreign import ccall unsafe "printFakeErrorMessage" printFakeErrorMessage :: IO CInt
foreign import ccall unsafe "openDialogAndLogResult" openDialogAndLogResult :: IO CInt

doSomeLog = do
  putStrLn "BRURHEHHADHDBD BDDd :)"

    -- My fake error
  _ <- printFakeErrorMessage

  return ()