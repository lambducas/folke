{-# OPTIONS_GHC -Wno-deprecations #-}
module Frontend.Main where

import Frontend.Types
import Frontend.BuildUI ( buildUI )
import Frontend.HandleEvent ( handleEvent )
import Frontend.Themes ( customLightTheme )
import Frontend.Communication (startCommunication)
import Frontend.Preferences (readPreferences, readPersistentState)
import Frontend.Helper.General (getAssetBasePath)

import Monomer
import Monomer.Main.Platform (getPlatform)
import Control.Concurrent (newChan)
import Data.Maybe (fromMaybe)
import Control.Concurrent.STM (newTChanIO)
import System.FilePath ((</>))
import Data.Text (pack)

main :: IO ()
main = do
  os <- getPlatform
  assetBasePath <- getAssetBasePath

  -- Read preferences from file or use default on error/first time opening app
  readPrefs <- readPreferences
  let prefs = fromMaybe defaultPrefs readPrefs

  -- Read app state from file or use default on error/first time opening app
  readState <- readPersistentState
  let state = fromMaybe defaultState readState

  -- Start communication between frontend and backend
  currentFrontendChan <- newChan
  currentBackendChan <- newChan
  _ <- startCommunication currentFrontendChan currentBackendChan

  channel <- newTChanIO
  let env = AppEnv channel

  let createModel = model prefs state currentFrontendChan currentBackendChan
  let createHandleEvent = handleEvent os env
  let createBuildUI = buildUI os
  let createConfig = config assetBasePath prefs state

  -- Start Monomer application
  startApp createModel createHandleEvent createBuildUI createConfig

  where
    -- Application configuration
    config assetBasePath prefs state = [
      appWindowTitle "Folke - A Proof Editor",
      appScaleFactor (_appScale prefs),
      appWindowState (_windowMode state),
      appTheme customLightTheme,

      appInitEvent AppInit,
      appExitEvent AppBeforeExit,
      appResizeEvent AppResize,
      appModelFingerprint show,

      appFontDef "Regular" (pack (assetBasePath </> "assets/fonts/MPLUS1p/MPLUS1p-Regular.ttf")),
      appFontDef "Medium" (pack (assetBasePath </> "assets/fonts/MPLUS1p/MPLUS1p-Medium.ttf")),
      appFontDef "Bold" (pack (assetBasePath </> "assets/fonts/MPLUS1p/MPLUS1p-Bold.ttf")),

      appFontDef "Dyslexic" (pack (assetBasePath </> "assets/fonts/Dyslexic/open-dyslexic.ttf")),

      appFontDef "Roboto_Regular" (pack (assetBasePath </> "assets/fonts/Roboto/Roboto-Regular.ttf")),
      appFontDef "Roboto_Medium" (pack (assetBasePath </> "assets/fonts/Roboto/Roboto-Medium.ttf")),
      appFontDef "Roboto_Bold" (pack (assetBasePath </> "assets/fonts/Roboto/Roboto-Bold.ttf")),

      appFontDef "Comic_Sans_Thin" (pack (assetBasePath </> "assets/fonts/ldfcomicsans-font/Ldfcomicsanshairline-5PmL.ttf")),
      appFontDef "Comic_Sans_Regular" (pack (assetBasePath </> "assets/fonts/ldfcomicsans-font/Ldfcomicsanslight-6dZo.ttf")),
      appFontDef "Comic_Sans_Medium" (pack (assetBasePath </> "assets/fonts/ldfcomicsans-font/Ldfcomicsans-jj7l.ttf")),
      appFontDef "Comic_Sans_Bold" (pack (assetBasePath </> "assets/fonts/ldfcomicsans-font/Ldfcomicsansbold-zgma.ttf")),

      appFontDef "Symbol_Regular" (pack (assetBasePath </> "assets/fonts/JuliaMono/JuliaMono-Regular.ttf")),
      appFontDef "Symbol_Medium" (pack (assetBasePath </> "assets/fonts/JuliaMono/JuliaMono-Medium.ttf")),
      appFontDef "Symbol_Bold" (pack (assetBasePath </> "assets/fonts/JuliaMono/JuliaMono-Bold.ttf")),

      appFontDef "Remix" (pack (assetBasePath </> "assets/fonts/remixicon.ttf"))
      ]
      
    -- Initial states
    model prefs state currentFrontendChan currentBackendChan = AppModel {
      _openMenuBarItem = Nothing,
      _contextMenu = ContextMenu {
        _ctxOpen = False,
        _ctxActions = []
      },
      _fileSearcher = FileSearcher {
        _fsOpen = False,
        _fsInput = "",
        _fsSelected = 0,
        _fsAllFiles = []
      },
      _ruleGuidePopup = Nothing,
      _udrPopup = False,

      _filesInDirectory = Just $ LoadedFiles [] [],
      _confirmActionPopup = Nothing,
      _exportOptionsPopup = ExportOptions {
        _eoOpen = False,
        _eoTitle = "",
        _eoStatus = ExportIdle,
        _eoLatexCompiler = Right ()
      },

      _frontendChan = currentFrontendChan,
      _backendChan = currentBackendChan,
      _proofStatus = Nothing,

      _preferences = prefs,
      _persistentState = state
    }

    -- Default preferences for new users
    defaultPrefs = Preferences {
      _selectedTheme = Light,
      _selectNormalFont = ["Regular", "Medium", "Bold"],
      _normalFont = "Regular",
      _logicFont = "Symbol_Regular",
      _fontSize = 16,
      _appScale = 0.8,
      _replaceAEInFormula = False,
      _autoCheckProofTracker = AutoCheckProofTracker { _acpEnabled = True },
      _warningMessageSeverity = 1
    }

    -- Default application state for new users
    defaultState = PersistentState {
      _firstTime = True,
      _windowMode = MainWindowNormal (1200, 800),
      _workingDir = Nothing,
      _currentFile = Nothing,
      _openFiles = [],
      _tmpLoadedFiles = [],
      _fileExplorerOpen = False,
      _fileExplorerWidth = 0.27,
      _rulesSidebarOpen = True,
      _rulesSidebarWidth = 0.82
    }
