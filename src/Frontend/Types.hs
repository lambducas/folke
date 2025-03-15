{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Frontend.Types where

import Monomer
import Data.Text (Text)
import Control.Concurrent (Chan)
import Control.Lens ( makeLenses )
import Shared.Messages ( BackendMessage, FrontendMessage )

type SymbolDict = [(Text, Text)]
type FormulaPath = [Int]

data FESequent = FESequent {
  _premises :: [FEFormula],
  _conclusion :: FEFormula,
  _steps :: [FEStep]
} deriving (Eq, Show)

type FEFormula = Text

data FEStep
  = Line {
    _statement :: Text,
    _rule :: Text
  }
  | SubProof [FEStep]
  deriving (Eq, Show)

data File = File {
  _path :: FilePath,
  _content :: Text,
  _parsedSequent :: FESequent,
  _isEdited :: Bool
} deriving (Eq, Show)

data AppModel = AppModel {
  _openMenuBarItem :: Maybe Integer,

  _newFilePopupOpen :: Bool,
  _newFileName :: Text,
  _filesInDirectory :: [FilePath],
  _tmpLoadedFiles :: [File],
  _openFiles :: [FilePath],
  _currentFile :: Maybe FilePath,
  _confirmDeletePopup :: Bool,
  _confirmDeleteTarget :: Maybe FilePath,

  _frontendChan :: Chan FrontendMessage,
  _backendChan :: Chan BackendMessage,
  _proofStatus :: Maybe Bool,

  _selectedTheme :: Theme
} deriving (Eq, Show)

instance Show (Chan a) where
  show :: Chan a -> String
  show _ = ""

data AppEvent
  = NoEvent
  | AppInit

  -- Menu bar
  | SetOpenMenuBarItem (Maybe Integer)

  -- Focus
  | NextFocus Int
  | FocusOnKey WidgetKey

  -- Handle premises
  | EditPremise Int Text
  | RemovePremise Int
  | AddPremise

  -- Handle conclusion
  | EditConclusion Text

  -- Handle proof
  | AddLine
  | AddSubProof
  | InsertLineAfter FormulaPath
  | InsertSubProofAfter FormulaPath
  | RemoveLine FormulaPath
  | EditLine FormulaPath Int Text
  | SwitchLineToSubProof FormulaPath
  | SwitchSubProofToLine FormulaPath

  -- Handle files
  | SetFilesInDirectory [FilePath]
  | OpenFile FilePath
  | OpenFileSuccess File
  | CloseFile FilePath
  | CloseFileSuccess FilePath
  | CloseCurrentFile
  | SaveProof File
  | SaveProofSuccess File
  | SetCurrentFile FilePath

  -- Handle creation of proof
  | OpenCreateProofPopup
  | CreateEmptyProof Text

  -- Proof checking
  | CheckProof
  | BackendResponse BackendMessage

  -- Theme
  | SwitchTheme
  deriving (Eq, Show)

makeLenses 'File
makeLenses 'AppModel