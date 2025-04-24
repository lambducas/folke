{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE InstanceSigs #-}

module Shared.Messages where

import Logic.Abs (Sequent, Step)
import Backend.Environment
import Shared.FESequent (FESequent)

data FrontendMessage
    = CheckSequent Sequent
    | CheckStringSequent String
    | CheckFESequent FESequent
    | CheckStep Step
    | OtherFrontendMessage String
    deriving (Show, Eq)

data BackendMessage
    = SequentChecked FEResult
    | StringSequentChecked FEResult
    | StepChecked (Either String ())
    | OtherBackendMessage String
    deriving (Show, Eq)

instance Show (Result t) where
instance Eq (Result t) where

data FEResult
    = FEOk [FEErrorWhere]
    | FEError [FEErrorWhere] FEErrorWhere
    deriving (Eq, Show)

data FEErrorWhere
    = FEGlobal String
    | FELocal Ref String
    deriving (Eq)

instance Show FEErrorWhere where
    show :: FEErrorWhere -> String
    show (FEGlobal msg) = msg
    show (FELocal line msg) = show line ++ ": " ++ msg
