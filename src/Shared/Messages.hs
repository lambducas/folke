module Shared.Messages (
    FrontendMessage(..),
    BackendMessage(..)
) where

import Parser.Logic.Abs (Sequent, Step)

data FrontendMessage
    = CheckSequent Sequent
    | CheckStep Step
    | OtherFrontendMessage String
    deriving (Show, Eq)

data BackendMessage
    = SequentChecked (Either String Sequent)
    | StepChecked (Either String Step)
    | OtherBackendMessage String
    deriving (Show, Eq)
