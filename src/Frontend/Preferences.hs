module Frontend.Preferences where

import Frontend.Types
import Control.Lens ((^.))
import Control.Exception (try, SomeException)
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Directory (doesFileExist, XdgDirectory (..), getXdgDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.IO (readFile')

appName :: String
appName = "Folke"

getPreferencePath :: IO FilePath
getPreferencePath = do
  basePath <- getXdgDirectory XdgConfig ("./" <> appName)
  return $ basePath </> "preferences.json"

getPersistentStatePath :: IO FilePath
getPersistentStatePath = do
  basePath <- getXdgDirectory XdgState ("./" <> appName)
  return $ basePath </> "persistent_state.json"

getTmpBasePath :: IO FilePath
getTmpBasePath = do
  basePath <- getXdgDirectory XdgState ("./" <> appName)
  return $ basePath </> "_tmp"

-- Preferences are config data and should be stored
-- at the right place on the users system (location: ~/.config or %APPDATA%)
readPreferences :: IO (Maybe Preferences)
readPreferences = do
  preferencePath <- getPreferencePath
  exists <- doesFileExist preferencePath
  if exists then
    do
      tryC <- try (readFile' preferencePath) :: IO (Either SomeException String)
      case tryC of
        Left e -> do
          print e
          return Nothing
        Right c -> do
          return (decode (BL.pack c) :: Maybe Preferences)
  else
    do
      createDirectoryIfMissing True (takeDirectory preferencePath)
      _ <- try (writeFile preferencePath "") :: IO (Either SomeException ())
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
  preferencePath <- getPreferencePath
  result <- try (writeFile preferencePath json) :: IO (Either SomeException ())
  case result of
    Left e -> print e
    Right _ -> sendMsg messageOnSuccess

-- State is data that should persist between app restarts
-- but not portable or import enough to the user to be
-- stored with preferences (location: ~/.local/state or %LOCALAPPDATA%)
readPersistentState :: IO (Maybe PersistentState)
readPersistentState = do
  persistentStatePath <- getPersistentStatePath
  exists <- doesFileExist persistentStatePath
  if exists then
    do
      tryC <- try (readFile' persistentStatePath) :: IO (Either SomeException String)
      case tryC of
        Left e -> do
          print e
          return Nothing
        Right c -> do
          return (decode (BL.pack c) :: Maybe PersistentState)
  else
    do
      createDirectoryIfMissing True (takeDirectory persistentStatePath)
      _ <- try (writeFile persistentStatePath "") :: IO (Either SomeException ())
      return Nothing

savePersistentState :: AppModel -> t -> (t -> IO ()) -> IO ()
savePersistentState model messageOnSuccess sendMsg = do
  let json = (BL.unpack . encodePretty) (model ^. persistentState)
  persistentStatePath <- getPersistentStatePath
  result <- try (writeFile persistentStatePath json) :: IO (Either SomeException ())
  case result of
    Left e -> print e
    Right _ -> sendMsg messageOnSuccess

savePrefAndState :: AppModel -> t -> (t -> IO ()) -> IO ()
savePrefAndState model messageOnSuccess sendMsg = savePreferences model messageOnSuccess (const $ savePersistentState model messageOnSuccess sendMsg)