{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Frontend.Types where

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
  _name :: Text,
  _subname :: Text,
  _content :: Text,
  _parsedSequent :: FESequent,
  _isEdited :: Bool
} deriving (Eq, Show)

data AppModel = AppModel {
  _newFilePopupOpen :: Bool,
  _newFileName :: Text,
  _filesInDirectory :: [FilePath],
  _tmpLoadedFiles :: [File],
  _openFiles :: [FilePath],
  _currentFile :: Maybe FilePath,

  _frontendChan :: Chan FrontendMessage,
  _backendChan :: Chan BackendMessage,
  _proofStatus :: Maybe Bool
} deriving (Eq, Show)

instance Show (Chan a) where
  show :: Chan a -> String
  show _ = ""

data AppEvent
  = AppInit
  | NextFocus Int

  | AddLine
  | AddSubProof
  | InsertLineAfter FormulaPath
  | InsertSubProofAfter FormulaPath
  | RemoveLine FormulaPath
  | EditLine FormulaPath Int Text
  | SwitchLineToSubProof FormulaPath
  | SwitchSubProofToLine FormulaPath

  | SetFilesInDirectory [FilePath]
  | OpenFile FilePath
  | OpenFileSuccess File
  | CloseFile FilePath
  | CloseCurrentFile
  | SaveProof File
  | SaveProofSuccess File
  | SetCurrentFile FilePath

  | OpenCreateProofPopup
  | CreateEmptyProof Text

  | CheckProof
  | BackendResponse BackendMessage
  deriving (Eq, Show)

makeLenses 'File
makeLenses 'AppModel