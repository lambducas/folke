module Shared.Messages (
    FrontendMessage(..),
    BackendMessage(..)
) where

import Logic.Abs (Sequent, Step)

data FrontendMessage
    = CheckSequent Sequent
    | CheckStringSequent String
    | CheckStep Step
    | OtherFrontendMessage String
    deriving (Show, Eq)

data BackendMessage
    = SequentChecked (Either String ())
    | StringSequentChecked (Either String ())
    | StepChecked (Either String ())
    | OtherBackendMessage String
    deriving (Show, Eq)
