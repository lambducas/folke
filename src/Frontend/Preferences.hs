module Frontend.Preferences (
  preferencePath,
  readPreferences,
  readAndApplyPreferences,
  savePreferences
) where

import Frontend.Types
import Control.Lens ((^.))
import Control.Exception (try, SomeException)
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Directory (doesFileExist)

preferencePath :: FilePath
preferencePath = "Settings.json"

readPreferences :: IO (Maybe Preferences)
readPreferences = do
  exists <- doesFileExist preferencePath
  if exists then
    do
      c <- readFile preferencePath
      return (decode (BL.pack c) :: Maybe Preferences)
  else
    do
      writeFile preferencePath ""
      return Nothing

readAndApplyPreferences :: (AppEvent -> IO ()) -> IO ()
readAndApplyPreferences sendMsg = do
  mJson <- readPreferences
  case mJson of
    Nothing -> return ()
    Just json -> sendMsg (ReadPreferences_ json)

savePreferences :: AppModel -> t -> (t -> IO ()) -> IO ()
savePreferences model messageOnSuccess sendMsg = do
  let json = (BL.unpack . encodePretty) (model ^. preferences)
  result <- try (writeFile preferencePath json) :: IO (Either SomeException ())
  case result of
    Left e -> print e
    Right _ -> sendMsg messageOnSuccess