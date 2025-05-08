{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Frontend.Types (
  module Frontend.Types,
  module Shared.FESequent
) where

import Shared.FESequent
import Monomer
import Data.Text (Text)
import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent (Chan)
import Control.Lens ( makeLenses )
import Shared.Messages ( BackendMessage, FrontendMessage, FEResult )
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map as Map

data RuleMetaData = RuleMetaData {
  _nrArguments :: Integer,
  _argumentLabels :: [Text]
}

type FormulaPath = [Int]

data HistoryEvent
  = HMultiple [HistoryEvent]
  | HUpdateSequent FESequent FESequent
  -- | HRemoveStep Bool FormulaPath FEStep
  -- | HInsertStep Bool FormulaPath FEStep
  | HEditStep FormulaPath FEStep FEStep
  -- I would like to only need to store target and destination path
  -- but I might have to store the whole proof :/
  -- | HMoveStep FormulaPath FormulaPath
  -- | HMoveStep [FEStep] [FEStep]
  -- | HRemovePremise Int Text
  -- | HInsertPremise Int Text
  | HEditPremise Int Text Text
  | HEditConclusion Text Text
  deriving (Eq, Show)

data History = History {
  _hState :: [HistoryEvent],
  _hIndex :: Int
} deriving (Eq, Show)

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
    _isEdited :: Bool,
    _history :: History
  }
  | TemporaryProofFile {
    _path :: FilePath,
    _parsedSequent :: Maybe FESequent,
    _isEdited :: Bool,
    _history :: History
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
  _appScale :: Double,
  _replaceAEInFormula :: Bool,
  _autoCheckProofTracker :: AutoCheckProofTracker
} deriving (Eq, Show)

data PersistentState = PersistentState {
  _firstTime :: Bool,
  _windowMode :: MainWindowState,
  _workingDir :: Maybe FilePath,
  _currentFile :: Maybe FilePath,
  _openFiles :: [FilePath],
  _tmpLoadedFiles :: [File],
  _fileExplorerOpen :: Bool,
  _fileExplorerWidth :: Double,
  _rulesSidebarOpen :: Bool,
  _rulesSidebarWidth :: Double,
  _proofStatusBarHeight :: Double
} deriving (Eq, Show)

type ContextMenuActions = [(Text, Text, AppEvent, Bool)]

data ContextMenu = ContextMenu {
  _ctxOpen :: Bool,
  _ctxActions :: ContextMenuActions
} deriving (Eq, Show)

data ConfirmActionData = ConfirmActionData {
  _cadTitle :: Text,
  _cadBody :: Text,
  _cadAction :: AppEvent
} deriving (Eq, Show)

newtype AutoCheckProofTracker = AutoCheckProofTracker {
  _acpEnabled :: Bool
} deriving (Eq, Show)

data LoadedFiles = LoadedFiles {
  _lFiles :: [FilePath],
  _lDirectories :: [FilePath]
} deriving (Show, Eq)

data FileSearcher = FileSearcher {
  _fsOpen :: Bool,
  _fsInput :: Text,
  _fsSelected :: Int,
  _fsAllFiles :: [FilePath]
} deriving (Show, Eq)

data AppModel = AppModel {
  _openMenuBarItem :: Maybe Integer,
  _contextMenu :: ContextMenu,
  _confirmActionPopup :: Maybe ConfirmActionData,
  _fileSearcher :: FileSearcher,

  _filesInDirectory :: Maybe LoadedFiles,

  _frontendChan :: Chan FrontendMessage,
  _backendChan :: Chan BackendMessage,
  _proofStatus :: Maybe FEResult,

  _preferences :: Preferences,
  _persistentState :: PersistentState
} deriving (Eq, Show)

instance Show (Chan a) where
  show :: Chan a -> String
  show _ = ""

instance Eq (ProducerHandler AppEvent) where
  (==) :: ProducerHandler AppEvent -> ProducerHandler AppEvent -> Bool
  _ == _ = False

instance Show (ProducerHandler AppEvent) where
  show :: ProducerHandler AppEvent -> String
  show _ = ""

data AppEvent
  = NoEvent
  | AppRunProducer (ProducerHandler AppEvent)
  | Undo
  | Redo
  | AppInit
  | AppBeforeExit
  | ExitApp
  | AppResize MainWindowState
  | CopyToClipboard Text
  | SimulateTextInput Text
  | Print String

  -- File searcher
  | OpenFileSearcher
  | CloseFileSearcher
  | ChangeFileSearcherIndex Int WidgetKey
  | ResetFileSearcherIndex
  | SetAllFilesInFileSearcher [FilePath]

  -- Confirm action
  | OpenConfirmAction ConfirmActionData
  | CloseConfirmAction AppEvent

  -- Menu bar
  | SetOpenMenuBarItem (Maybe Integer)

  -- Context menu
  | OpenContextMenu ContextMenuActions
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
  | InsertLineAfter Bool FormulaPath
  | InsertLineBefore Bool FormulaPath
  | InsertSubProofAfter Bool FormulaPath
  | InsertSubProofBefore Bool FormulaPath
  | RemoveLine Bool FormulaPath
  | EditFormula FormulaPath Text
  | EditRuleName FormulaPath Text
  | EditRuleArgument FormulaPath Int Text
  | SwitchLineToSubProof FormulaPath WidgetKey
  | SwitchSubProofToLine FormulaPath WidgetKey
  | MovePathToPath FormulaPath FormulaPath

  -- Handle files
  | OpenPreferences
  | OpenGuide
  | OpenWelcome
  | ToggleFileExplorer
  | ToggleRulesSidebar
  | RefreshExplorer
  | OpenSetWorkingDir
  | SetWorkingDir FilePath
  | SetFilesInDirectory (Maybe LoadedFiles)
  | OpenFileFromFileSystem
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
  | MoveTab Int Int

  -- Handle creation of proof
  | CreateEmptyProof

  -- Proof checking
  | CheckProof File
  | CheckCurrentProof
  | BackendResponse BackendMessage
  | AutoCheckProof

  -- Theme
  | SwitchTheme

  -- Preferences
  | UpdateFont [String]
  | ReadPreferences
  | ReadPreferences_ Preferences
  | SavePreferences
  | ResetFontSize
  | ResetAppScale

  -- Export to LaTeX
  | ExportToLaTeX
  | ExportToPDF
  | ExportSuccess Text
  | ExportError Text
  deriving (Eq, Show)

makeLenses 'PersistentState
makeLenses 'Preferences
makeLenses 'History
makeLenses 'ProofFile
makeLenses 'ConfirmActionData
makeLenses 'ContextMenu
makeLenses 'AppModel
makeLenses 'AutoCheckProofTracker
makeLenses 'FileSearcher
makeLenses 'LoadedFiles

newtype AppEnv = AppEnv {
  _envChannel :: TChan ()
}

makeLenses 'AppEnv

-- | File extension used for proofs
feFileExts :: [String]
feFileExts = ["folke", "ndp", "json"]

$(deriveJSON defaultOptions ''SelectableTheme)
$(deriveJSON defaultOptions ''HistoryEvent)
$(deriveJSON defaultOptions ''History)
$(deriveJSON defaultOptions ''File)
$(deriveJSON defaultOptions ''MainWindowState)
$(deriveJSON defaultOptions ''AutoCheckProofTracker)
$(deriveJSON defaultOptions ''Preferences)
$(deriveJSON defaultOptions ''PersistentState)

ruleMetaDataMap :: Map.Map Text RuleMetaData
ruleMetaDataMap = Map.fromList [
    ("assume", RuleMetaData {_nrArguments = 0, _argumentLabels = []}),
    ("fresh", RuleMetaData {_nrArguments = 0, _argumentLabels = []}),
    ("copy", RuleMetaData {_nrArguments = 1, _argumentLabels = [""]}),
    ("AndI", RuleMetaData {_nrArguments = 2, _argumentLabels = ["", ""]}),
    ("AndEL", RuleMetaData {_nrArguments = 1, _argumentLabels = [""]}),
    ("AndER", RuleMetaData {_nrArguments = 1, _argumentLabels = [""]}),
    ("OrIL", RuleMetaData {_nrArguments = 1, _argumentLabels = [""]}),
    ("OrIR", RuleMetaData {_nrArguments = 1, _argumentLabels = [""]}),
    ("OrE", RuleMetaData {_nrArguments = 3, _argumentLabels = ["", "", ""]}),
    ("ImplI", RuleMetaData {_nrArguments = 1, _argumentLabels = [""]}),
    ("ImplE", RuleMetaData {_nrArguments = 2, _argumentLabels = ["", ""]}),
    ("NotI", RuleMetaData {_nrArguments = 1, _argumentLabels = [""]}),
    ("NotE", RuleMetaData {_nrArguments = 2, _argumentLabels = ["", ""]}),
    ("BotE", RuleMetaData {_nrArguments = 1, _argumentLabels = [""]}),
    ("NotNotI", RuleMetaData {_nrArguments = 1, _argumentLabels = [""]}),
    ("NotNotE", RuleMetaData {_nrArguments = 1, _argumentLabels = [""]}),
    ("MT", RuleMetaData {_nrArguments = 2, _argumentLabels = ["", ""]}),
    ("PBC", RuleMetaData {_nrArguments = 1, _argumentLabels = [""]}),
    ("LEM", RuleMetaData {_nrArguments = 0, _argumentLabels = []}),
    ("EqI", RuleMetaData {_nrArguments = 0, _argumentLabels = []}),
    ("EqE", RuleMetaData {_nrArguments = 3, _argumentLabels = ["", "", ""]}),
    -- ("EqE", RuleMetaData {_nrArguments = 3, _argumentLabels = ["", "", "𝝓(u)≡"]}),
    ("AllE", RuleMetaData {_nrArguments = 2, _argumentLabels = ["", ""]}),
    ("AllI", RuleMetaData {_nrArguments = 1, _argumentLabels = [""]}),
    ("SomeE", RuleMetaData {_nrArguments = 2, _argumentLabels = ["", ""]}),
    ("SomeI", RuleMetaData {_nrArguments = 1, _argumentLabels = [""]})
  ]

symbolsList :: [Text]
symbolsList = [
    "¬",
    "→",
    "∧",
    "∨",
    "⊥",
    "∀",
    "∃",
    "⊬",
    "⊢",
    "₁",
    "₂",
    "₃",
    "₄",
    "₅",
    "₆",
    "₇",
    "₈",
    "₉",
    "₀"
  ]

visualRuleNames :: [(Text, Text)]
visualRuleNames = visualRuleNames0 ++ visualRuleNames1

visualRuleNames0 :: [(Text, Text)]
visualRuleNames0 = [
    ("assume", "assume"),
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