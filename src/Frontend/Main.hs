-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Frontend.Main where

import Frontend.Types
import Frontend.BuildUI ( buildUI )
import Frontend.HandleEvent ( handleEvent )
import Frontend.Themes ( customLightTheme, customDarkTheme )

import Monomer
import Control.Concurrent (newChan)
import Frontend.Communication (startCommunication)

main :: IO ()
main = do
  currentFrontendChan <- newChan
  currentBackendChan <- newChan
  _ <- startCommunication currentFrontendChan currentBackendChan
  startApp (model currentFrontendChan currentBackendChan) handleEvent buildUI config

  where
    config = [
      appWindowTitle "Proof Editor",
      appWindowIcon "./assets/images/icon.png",
      appTheme customLightTheme,
      -- appTheme darkTheme,

      appFontDef "Regular" "./assets/fonts/MPLUS1p/MPLUS1p-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/MPLUS1p/MPLUS1p-Medium.ttf",
      appFontDef "Bold" "./assets/fonts/MPLUS1p/MPLUS1p-Bold.ttf",

      appFontDef "Dyslexic" "assets/fonts/Dyslexic/open-dyslexic.ttf",

      appFontDef "Roboto_Regular" "./assets/fonts/Roboto/Roboto-Regular.ttf",
      appFontDef "Roboto_Medium" "./assets/fonts/Roboto/Roboto-Medium.ttf",
      appFontDef "Roboto_Bold" "./assets/fonts/Roboto/Roboto-Bold.ttf",

      appFontDef "Comic_Sans_Medium" "assets/fonts/ldfcomicsans-font/Ldfcomicsans-jj7l.ttf",
      appFontDef "Comic_Sans_Bold" "assets/fonts/ldfcomicsans-font/Ldfcomicsansbold-zgma.ttf",
      appFontDef "Comic_Sans_Thin" "assets/fonts/ldfcomicsans-font/Ldfcomicsanshairline-5PmL.ttf",
      appFontDef "Comic_Sans_Regular" "assets/fonts/ldfcomicsans-font/Ldfcomicsanslight-6dZo.ttf",

      appFontDef "Symbol_Regular" "./assets/fonts/JuliaMono/JuliaMono-Regular.ttf",
      appFontDef "Symbol_Medium" "./assets/fonts/JuliaMono/JuliaMono-Medium.ttf",
      appFontDef "Symbol_Bold" "./assets/fonts/JuliaMono/JuliaMono-Bold.ttf",

      appFontDef "Remix" "./assets/fonts/remixicon.ttf",

      appInitEvent AppInit,
      appModelFingerprint show
      ]
      
    -- Initial states
    model currentFrontendChan currentBackendChan = AppModel {
      _openMenuBarItem = Nothing,

      _newFileName = "",
      _newFilePopupOpen = False,
      _filesInDirectory = [],
      _tmpLoadedFiles = [],
      _openFiles = [],
      _currentFile = Nothing,
      _confirmDeletePopup = False,
      _confirmDeleteTarget = Nothing,

      _frontendChan = currentFrontendChan,
      _backendChan = currentBackendChan,
      _proofStatus = Nothing,

      _selectedTheme = customDarkTheme,

      _selectNormalFont = ["Regular","Medium","Bold"],
      _normalFont = "Regular",
      _logicFont = "Symbol_Regular"
    }