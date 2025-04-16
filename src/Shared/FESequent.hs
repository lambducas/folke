{-# LANGUAGE TemplateHaskell #-}

module Shared.FESequent where

import Data.Text ( Text )
import Data.Aeson ( defaultOptions )
import Data.Aeson.TH ( deriveJSON )
import Control.Lens ( makeLenses )

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

makeLenses 'FESequent

$(deriveJSON defaultOptions ''FEStep)
$(deriveJSON defaultOptions ''FESequent)