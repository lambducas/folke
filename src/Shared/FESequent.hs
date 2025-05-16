{-# LANGUAGE TemplateHaskell #-}

module Shared.FESequent where

import Data.Text ( Text )
import Data.Aeson ( defaultOptions )
import Data.Aeson.TH ( deriveJSON )
import Control.Lens ( makeLenses )

data FEDocument = FEDocument {
  _fedUserDefinedRules :: Maybe [FEUserDefinedRule],
  _sequent :: FESequent
} deriving (Show, Eq)

data FEUserDefinedRule = FEUserDefinedRule {
  _udrName :: Text,
  _udrPath :: FilePath,
  _udrInput :: Maybe [FEFormula],
  _udrOutput :: Maybe FEFormula
} deriving (Show, Eq)

data FESequent = FESequent {
  _premises :: [FEFormula],
  _conclusion :: FEFormula,
  _steps :: [FEStep]
} deriving (Eq, Show)

type FEFormula = Text

data FEStep
  = Line {
    _statement :: Text,
    _rule :: Text,
    _usedArguments :: Int,
    _arguments :: [Text]
  }
  | SubProof [FEStep]
  deriving (Eq, Show)

makeLenses 'FEUserDefinedRule
makeLenses 'FESequent
makeLenses 'FEDocument

$(deriveJSON defaultOptions ''FEStep)
$(deriveJSON defaultOptions ''FESequent)
$(deriveJSON defaultOptions ''FEUserDefinedRule)
$(deriveJSON defaultOptions ''FEDocument)