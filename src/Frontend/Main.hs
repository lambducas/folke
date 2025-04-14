module Frontend.Main where

import Frontend.Types
import Frontend.BuildUI ( buildUI )
import Frontend.HandleEvent ( handleEvent )
import Frontend.Themes ( customLightTheme )
import Frontend.Communication (startCommunication)
import Frontend.Preferences (readPreferences)

import Monomer
import Control.Concurrent (newChan)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  -- Read preferences from file or use default on error
  readPrefs <- readPreferences
  let defaultPrefs = Preferences {
    _selectedTheme = Dark,
    _selectNormalFont = ["Regular", "Medium", "Bold"],
    _normalFont = "Regular",
    _logicFont = "Symbol_Regular",
    _fontSize = 16,

    _windowMode = MainWindowNormal (800, 600),
    _workingDir = Nothing, --Just "./myProofs",
    _currentFile = Nothing,
    _openFiles = [],
    _tmpLoadedFiles = [],
    _fileExplorerOpen = True,
    _fileExplorerWidth = 300,
    _rulesSidebarOpen = True,
    _rulesSidebarWidth = 300
  }
  let prefs = fromMaybe defaultPrefs readPrefs

  -- Start communication between frontend and backend
  currentFrontendChan <- newChan
  currentBackendChan <- newChan
  _ <- startCommunication currentFrontendChan currentBackendChan

  -- Start Monomer application
  startApp (model prefs currentFrontendChan currentBackendChan) handleEvent buildUI (config prefs)

  where
    config _prefs = [
      --appScaleFactor (_appScale prefs),

      appWindowState (_windowMode _prefs),
      appWindowTitle "Proof Editor",
      appWindowIcon "./assets/images/icon.png",
      appTheme customLightTheme,

      appFontDef "Regular" "./assets/fonts/MPLUS1p/MPLUS1p-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/MPLUS1p/MPLUS1p-Medium.ttf",
      appFontDef "Bold" "./assets/fonts/MPLUS1p/MPLUS1p-Bold.ttf",

      appFontDef "Dyslexic" "assets/fonts/Dyslexic/open-dyslexic.ttf",

      appFontDef "Roboto_Regular" "./assets/fonts/Roboto/Roboto-Regular.ttf",
      appFontDef "Roboto_Medium" "./assets/fonts/Roboto/Roboto-Medium.ttf",
      appFontDef "Roboto_Bold" "./assets/fonts/Roboto/Roboto-Bold.ttf",

      appFontDef "Comic_Sans_Thin" "assets/fonts/ldfcomicsans-font/Ldfcomicsanshairline-5PmL.ttf",
      appFontDef "Comic_Sans_Regular" "assets/fonts/ldfcomicsans-font/Ldfcomicsanslight-6dZo.ttf",
      appFontDef "Comic_Sans_Medium" "assets/fonts/ldfcomicsans-font/Ldfcomicsans-jj7l.ttf",
      appFontDef "Comic_Sans_Bold" "assets/fonts/ldfcomicsans-font/Ldfcomicsansbold-zgma.ttf",

      appFontDef "Symbol_Regular" "./assets/fonts/JuliaMono/JuliaMono-Regular.ttf",
      appFontDef "Symbol_Medium" "./assets/fonts/JuliaMono/JuliaMono-Medium.ttf",
      appFontDef "Symbol_Bold" "./assets/fonts/JuliaMono/JuliaMono-Bold.ttf",

      appFontDef "Remix" "./assets/fonts/remixicon.ttf",

      appInitEvent AppInit,
      appExitEvent AppBeforeExit,
      appResizeEvent AppResize,
      appModelFingerprint show
      ]
      
    -- Initial states
    model prefs currentFrontendChan currentBackendChan = AppModel {
      _openMenuBarItem = Nothing,
      _contextMenu = ContextMenu {
        _ctxOpen = False,
        _ctxActions = []
      },

      _filesInDirectory = Just [],
      _confirmActionPopup = Nothing,

      _frontendChan = currentFrontendChan,
      _backendChan = currentBackendChan,
      _proofStatus = Nothing,

      _preferences = prefs
    }