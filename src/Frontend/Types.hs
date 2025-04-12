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

  _windowMode :: MainWindowState,
  _workingDir :: Maybe FilePath,
  _currentFile :: Maybe FilePath,
  _openFiles :: [FilePath],
  _tmpLoadedFiles :: [File],
  _fileExplorerOpen :: Bool,
  _fileExplorerWidth :: Double,
  _rulesSidebarOpen :: Bool
} deriving (Eq, Show)

data ContextMenu = ContextMenu {
  _ctxOpen :: Bool,
  _ctxFilePath :: Maybe FilePath
} deriving (Eq, Show)

data ConfirmActionData = ConfirmActionData {
  _cadTitle :: Text,
  _cadBody :: Text,
  _cadAction :: AppEvent
} deriving (Eq, Show)

data AppModel = AppModel {
  _openMenuBarItem :: Maybe Integer,
  _contextMenu :: ContextMenu,

  _filesInDirectory :: Maybe [FilePath],
  _confirmActionPopup :: Maybe ConfirmActionData,

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
  | AppResize MainWindowState
  | CopyToClipboard Text
  | Print String

  -- Confirm action
  | OpenConfirmAction ConfirmActionData
  | CloseConfirmAction AppEvent

  -- Menu bar
  | SetOpenMenuBarItem (Maybe Integer)

  -- Context menu
  | OpenContextMenu FilePath
  | CloseContextMenu
  | DeleteFilePath FilePath
  | OpenInExplorer FilePath

  -- Focus
  | NextFocus Int
  | FocusOnKey WidgetKey

  -- Handle premises
  | EditPremise Int Text
  | RemovePremise Int
  | AddPremise Int

  -- Handle conclusion
  | EditConclusion Text

  -- Handle proof
  | AddLine
  | AddSubProof
  | InsertLineAfter FormulaPath
  | InsertLineBefore FormulaPath
  | InsertSubProofAfter FormulaPath
  | InsertSubProofBefore FormulaPath
  | RemoveLine FormulaPath
  | EditFormula FormulaPath Text
  | EditRuleName FormulaPath Text
  | EditRuleArgument FormulaPath Int Text
  | SwitchLineToSubProof FormulaPath WidgetKey
  | SwitchSubProofToLine FormulaPath WidgetKey

  -- Handle files
  | OpenPreferences
  | OpenGuide
  | ToggleFileExplorer
  | ToggleRulesSidebar
  | RefreshExplorer
  | OpenSetWorkingDir
  | SetWorkingDir FilePath
  | SetFilesInDirectory (Maybe [FilePath])
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
  | CreateEmptyProof

  -- Proof checking
  | CheckProof File
  | CheckCurrentProof
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
makeLenses 'ConfirmActionData
makeLenses 'ContextMenu
makeLenses 'AppModel

feFileExt :: String
feFileExt = "json"

$(deriveJSON defaultOptions ''SelectableTheme)
$(deriveJSON defaultOptions ''File)
$(deriveJSON defaultOptions ''MainWindowState)
$(deriveJSON defaultOptions ''Preferences)

ruleMetaDataMap :: Map.Map Text RuleMetaData
ruleMetaDataMap = Map.fromList [
    ("assume", RuleMetaData {_nrArguments = 0, _argumentLabels = []}),
    ("fresh", RuleMetaData {_nrArguments = 0, _argumentLabels = []}),
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

visualRuleNames :: [(Text, Text)]
visualRuleNames = visualRuleNames0 ++ visualRuleNames1

visualRuleNames0 :: [(Text, Text)]
visualRuleNames0 = [
    ("assume", "assume"),
    ("fresh", "fresh"),
    ("copy", "copy"),
    ("AndI", "∧I"),
    ("AndEL", "∧EL"),
    ("AndER", "∧ER"),
    ("OrIL", "∨IL"),
    ("OrIR", "∨IR"),
    ("OrE", "∨E"),
    ("ImplI", "→I"),
    ("ImplE", "→E"),
    ("NotI", "¬I"),
    ("NotE", "¬E"),
    ("BotE", "⊥E"),
    ("NotNotI", "¬¬I"),
    ("NotNotE", "¬¬E"),
    ("MT", "MT"),
    ("PBC", "PBC"),
    ("LEM", "LEM")
  ]

visualRuleNames1 :: [(Text, Text)]
visualRuleNames1 = [
    ("fresh", "fresh"),
    ("EqI", "=I"),
    ("EqE", "=E"),
    ("AllE", "∀E"),
    ("AllI", "∀I"),
    ("SomeE", "∃E"),
    ("SomeI", "∃I")
  ]