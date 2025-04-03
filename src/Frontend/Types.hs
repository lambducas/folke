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
import Data.Aeson
import Data.Aeson.TH

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

data File
  = OtherFile {
    _path :: FilePath,
    _content :: Text
  }
  | PreferenceFile {
    _path :: FilePath,
    _isEdited :: Bool
  }
  | MarkdownFile {
    _path :: FilePath,
    _content :: Text
  }
  | ProofFile {
    _path :: FilePath,
    _content :: Text,
    _parsedSequent :: Maybe FESequent,
    _isEdited :: Bool
  }
  | TemporaryProofFile {
    _path :: FilePath,
    _parsedSequent :: Maybe FESequent,
    _isEdited :: Bool
  }
  deriving (Eq, Show)

data SelectableTheme = Light | Dark
  deriving (Eq, Show)

data Preferences = Preferences {
  _selectedTheme :: SelectableTheme,
  _selectNormalFont :: [String],
  _normalFont :: String,
  _logicFont :: String,
  _appScale :: Double,

  _workingDir :: Maybe FilePath
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
  _proofStatus :: Maybe (Either String ()),

  _preferences :: Preferences
} deriving (Eq, Show)

instance Show (Chan a) where
  show :: Chan a -> String
  show _ = ""

data AppEvent
  = NoEvent
  | AppInit
  | AppBeforeExit
  | ExitApp
  | Print String

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
  | SwitchLineToSubProof FormulaPath WidgetKey
  | SwitchSubProofToLine FormulaPath WidgetKey

  -- Handle files
  | RefreshExplorer
  | OpenSetWorkingDir
  | SetWorkingDir FilePath
  | SetFilesInDirectory [FilePath]
  | OpenFile FilePath
  | OpenFile_ FilePath FilePath
  | OpenFileSuccess File
  | CloseFile FilePath
  | CloseFileSuccess FilePath
  | CloseCurrentFile
  | SaveCurrentFile
  | SaveFile File
  | SaveFileSuccess File
  | SetCurrentFile FilePath

  -- Handle creation of proof
  | OpenCreateProofPopup
  | CreateEmptyProof

  -- Proof checking
  | CheckProof File
  | BackendResponse BackendMessage

  -- Theme
  | SwitchTheme

  -- Preferences
  | UpdateFont [String]
  | ReadPreferences
  | ReadPreferences_ Preferences
  | SavePreferences
  deriving (Eq, Show)

makeLenses 'Preferences
makeLenses 'ProofFile
makeLenses 'AppModel

feFileExt :: String
feFileExt = "json"

$(deriveJSON defaultOptions ''SelectableTheme)
$(deriveJSON defaultOptions ''Preferences)