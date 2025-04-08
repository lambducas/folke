{-# LANGUAGE TemplateHaskell #-}

module Shared.FESequent where

import Data.Text ( Text )
import Data.Aeson ( defaultOptions )
import Data.Aeson.TH ( deriveJSON )

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

$(deriveJSON defaultOptions ''FEStep)
$(deriveJSON defaultOptions ''FESequent)