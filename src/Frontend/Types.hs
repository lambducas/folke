{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Frontend.Types (
  module Frontend.Types,
  module Shared.FESequent
) where

import Shared.FESequent
import Monomer
import Data.Text (Text)
import Control.Concurrent (Chan)
import Control.Lens ( makeLenses )
import Shared.Messages ( BackendMessage, FrontendMessage, FEResult )
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map as Map

type SymbolDict = [(Text, Text)]
type FormulaPath = [Int]

data RuleMetaData = RuleMetaData {
  _nrArguments :: Integer,
  _argumentLabels :: [Text]
}

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
  _fontSize :: Double,

  _workingDir :: Maybe FilePath,
  _openFiles :: [FilePath],
  _tmpLoadedFiles :: [File]
} deriving (Eq, Show)

data AppModel = AppModel {
  _openMenuBarItem :: Maybe Integer,

  _newFilePopupOpen :: Bool,
  _newFileName :: Text,
  _filesInDirectory :: [FilePath],
  _currentFile :: Maybe FilePath,
  _confirmDeletePopup :: Bool,
  _confirmDeleteTarget :: Maybe FilePath,

  _frontendChan :: Chan FrontendMessage,
  _backendChan :: Chan BackendMessage,
  _proofStatus :: Maybe FEResult,

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
  | EditFormula FormulaPath Text
  | EditRuleName FormulaPath Text
  | EditRuleArgument FormulaPath Int Text
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
  | ResetFontSize

  -- Export to LaTeX
  | ExportToLaTeX
  | ExportSuccess Text
  | ExportError Text
  deriving (Eq, Show)

makeLenses 'Preferences
makeLenses 'ProofFile
makeLenses 'AppModel

feFileExt :: String
feFileExt = "json"

$(deriveJSON defaultOptions ''SelectableTheme)
$(deriveJSON defaultOptions ''File)
$(deriveJSON defaultOptions ''Preferences)

ruleMetaDataMap :: Map.Map Text RuleMetaData
ruleMetaDataMap = Map.fromList [
    ("assume", RuleMetaData {_nrArguments = 0, _argumentLabels = []}),
    ("free", RuleMetaData {_nrArguments = 0, _argumentLabels = []}),
    ("copy", RuleMetaData {_nrArguments = 1, _argumentLabels = []}),
    ("AndI", RuleMetaData {_nrArguments = 2, _argumentLabels = []}),
    ("AndEL", RuleMetaData {_nrArguments = 1, _argumentLabels = []}),
    ("AndER", RuleMetaData {_nrArguments = 1, _argumentLabels = []}),
    ("OrIL", RuleMetaData {_nrArguments = 1, _argumentLabels = []}),
    ("OrIR", RuleMetaData {_nrArguments = 1, _argumentLabels = []}),
    ("OrE", RuleMetaData {_nrArguments = 3, _argumentLabels = []}),
    ("ImplI", RuleMetaData {_nrArguments = 1, _argumentLabels = []}),
    ("ImplE", RuleMetaData {_nrArguments = 2, _argumentLabels = []}),
    ("NotI", RuleMetaData {_nrArguments = 1, _argumentLabels = []}),
    ("NotE", RuleMetaData {_nrArguments = 2, _argumentLabels = []}),
    ("BotE", RuleMetaData {_nrArguments = 1, _argumentLabels = []}),
    ("NotNotI", RuleMetaData {_nrArguments = 1, _argumentLabels = []}),
    ("NotNotE", RuleMetaData {_nrArguments = 1, _argumentLabels = []}),
    ("MT", RuleMetaData {_nrArguments = 2, _argumentLabels = []}),
    ("PBC", RuleMetaData {_nrArguments = 1, _argumentLabels = []}),
    ("LEM", RuleMetaData {_nrArguments = 0, _argumentLabels = []}),
    ("EqI", RuleMetaData {_nrArguments = 0, _argumentLabels = []}),
    ("EqE", RuleMetaData {_nrArguments = 3, _argumentLabels = []}),
    ("AllE", RuleMetaData {_nrArguments = 2, _argumentLabels = []}),
    ("AllI", RuleMetaData {_nrArguments = 1, _argumentLabels = []}),
    ("SomeE", RuleMetaData {_nrArguments = 2, _argumentLabels = []}),
    ("SomeI", RuleMetaData {_nrArguments = 2, _argumentLabels = []})
  ]