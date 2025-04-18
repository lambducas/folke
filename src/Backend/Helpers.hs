{- |
Module      : Backend.Helpers
Description : Helper functions for the proof checker
Copyright   : (c) Your Organization, 2023
License     : GPL-3

This module provides utility functions for the proof checker, including
error handling, result manipulation, and proof verification helpers.
-}
module Backend.Helpers
  (
    ---------------------------------------
    -- Error handling helpers
    ---------------------------------------
    throwError,
    throwTypeError,
    throwRuleError,
    throwArgError,
    throwUnknownError,
    catchError,
    mapError,
    mapErrorMsg,

    ---------------------------------------
    -- Result manipulation
    ---------------------------------------
    (<&&>),
    (<||>),
    resultMap,
    resultBind,
    resultApp,
    liftResult,
    sequence,

    ---------------------------------------
    -- Warning helpers
    ---------------------------------------
    addWarning,
    clearWarnings,

    ---------------------------------------
    -- Proof verification helpers
    ---------------------------------------
    hasNilConclusion,
    hasInvalidConclusion,
    filterNilConclusion,
    validateRefs,

    ---------------------------------------
    -- Utility functions
    ---------------------------------------
    identToString,
    getConclusion,
    convertToFEError,
    convertWarning,
    getErrorLine,
    maybeHead
  ) where

import Backend.Types (
    Severity(Low, Medium, High, Hint),
    WarningKind(StyleIssue, UnusedReference, GeneralWarning, IncompleteProof,
                PossibleSimplification, RedundantStep),
    Ref(..),
    Warning(..),
    Result(..),
    Error(..),
    ErrorKind(..),
    Env(..),
    Arg(ArgProof),
    Formula(Pred, Impl, And, Or, Not, All, Some, Eq, Bot, Nil),
    Proof(..),
    createTypeError,
    createRuleConcError,
    createRuleArgError,
    createUnknownError )

import Prelude hiding (sequence)

import Data.Maybe (fromMaybe)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Logic.Abs as Abs
import Shared.Messages

----------------------------------------------------------------------
-- Proof Verification Helpers
----------------------------------------------------------------------

-- | Check if a proof has a Nil conclusion
hasNilConclusion :: Proof -> Bool
hasNilConclusion (Proof _ _ Nil) = True
hasNilConclusion _ = False

-- | Check if a proof has an invalid conclusion compared to the expected one
hasInvalidConclusion :: Proof -> Proof -> Bool
hasInvalidConclusion (Proof _ _ actual) (Proof _ _ expected) =
    actual /= expected && actual /= Nil && isSameType actual expected
  where
    -- | Check if two formulas have the same structure
    isSameType :: Formula -> Formula -> Bool
    isSameType (Pred _) (Pred _) = True
    isSameType (Impl _ _) (Impl _ _) = True
    isSameType (And _ _) (And _ _) = True
    isSameType (Or _ _) (Or _ _) = True
    isSameType (Not _) (Not _) = True
    isSameType (All _ _) (All _ _) = True
    isSameType (Some _ _) (Some _ _) = True
    isSameType (Eq _ _) (Eq _ _) = True
    isSameType Bot Bot = True
    isSameType Nil Nil = True
    isSameType _ _ = False

-- | Filter Nil conclusions from the proof result
-- If a proof ends with Nil but has premises, use the last premise as the conclusion
filterNilConclusion :: Proof -> Proof
filterNilConclusion (Proof terms prems Nil) =
    case reverse prems of
        [] -> Proof terms prems Nil  -- No premises, keep Nil
        (lastPrem:rest) -> Proof terms (reverse rest) lastPrem  -- Use last premise as conclusion
filterNilConclusion proof = proof  -- Keep non-Nil conclusions as is

-- | Check for unused references and generate appropriate warnings
validateRefs :: Env -> Result ()
validateRefs env =
    -- Don't count references to entire proofs, which aren't meant to be cited
    let unusedRefs =
          [(ref, arg) |
           (ref, (count, arg)) <- Map.toList (refs env),
           count == 0,
           not (isProofReference arg)]
    in if null unusedRefs
       then Ok [] ()  -- No unused references found
       else
           let unusedRefsStr = List.intercalate ", " [show ref | (ref, _) <- unusedRefs]
           in if null unusedRefsStr
              then Ok [] ()
              else Ok [Warning {
                  warnLocation = Nothing,
                  warnSeverity = Low,
                  warnKind = StyleIssue "Unused references",
                  warnMessage = "Some references were defined but never used: " ++ unusedRefsStr,
                  warnSuggestion = Just "Consider removing unused references for cleaner proofs"
              }] ()
  where
    isProofReference (ArgProof _) = True  -- Don't count ArgProofs
    isProofReference _ = False

----------------------------------------------------------------------
-- Error Handling Helpers
----------------------------------------------------------------------

-- | Throw an error with the given error constructor and message
throwError :: (Env -> String -> Error) -> Env -> String -> Result a
throwError errCons env msg = Err [] env (errCons env msg)

-- | Throw a type error
throwTypeError :: Env -> String -> Result a
throwTypeError = throwError createTypeError

-- | Throw a rule conclusion error
throwRuleError :: Env -> String -> Result a
throwRuleError = throwError createRuleConcError

-- | Throw an argument error for a specific argument
throwArgError :: Env -> Integer -> String -> Result a
throwArgError env argNum msg = Err [] env (createRuleArgError env argNum msg)

-- | Throw an unknown error
throwUnknownError :: Env -> String -> Result a
throwUnknownError = throwError createUnknownError

-- | Catch an error and transform it into another result
catchError :: Result a -> (Error -> Result a) -> Result a
catchError (Ok warns val) _ = Ok warns val
catchError (Err warns env err) handler = case handler err of
    Ok newWarns val -> Ok (warns ++ newWarns) val
    Err newWarns newEnv newErr -> Err (warns ++ newWarns) newEnv newErr

-- | Map a function over the error in a Result
mapError :: (Error -> Error) -> Result a -> Result a
mapError _ (Ok warns val) = Ok warns val
mapError f (Err warns env err) = Err warns env (f err)

-- | Map a function over the error message in a Result
mapErrorMsg :: (String -> String) -> Result a -> Result a
mapErrorMsg f = mapError (\err -> err { errMessage = f (errMessage err) })

----------------------------------------------------------------------
-- Result Manipulation
----------------------------------------------------------------------

-- | Logical AND for Results - succeeds only if both succeed
(<&&>) :: Result Bool -> Result Bool -> Result Bool
(Ok warns1 val1) <&&> (Ok warns2 val2) = Ok (warns1 ++ warns2) (val1 && val2)
(Err warns env err) <&&> _ = Err warns env err
_ <&&> (Err warns env err) = Err warns env err

-- | Logical OR for Results - succeeds if either succeeds
(<||>) :: Result Bool -> Result Bool -> Result Bool
(Ok warns1 True) <||> _ = Ok warns1 True
(Ok warns1 False) <||> (Ok warns2 val2) = Ok (warns1 ++ warns2) val2
(Ok warns1 False) <||> (Err warns2 env err) = Err (warns1 ++ warns2) env err
(Err warns env err) <||> _ = Err warns env err

-- | Map a function over a Result
resultMap :: (a -> b) -> Result a -> Result b
resultMap = fmap

-- | Monadic bind for Results
resultBind :: Result a -> (a -> Result b) -> Result b
resultBind = (>>=)

-- | Apply a function in a Result to a value in a Result
resultApp :: Result (a -> b) -> Result a -> Result b
resultApp = (<*>)

-- | Lift a pure value into a Result
liftResult :: a -> Result a
liftResult = pure

-- | Sequence a list of Results into a Result of a list
sequence :: [Result a] -> Result [a]
sequence [] = Ok [] []
sequence (Ok warns x : xs) = case sequence xs of
  Ok moreWarns xs' -> Ok (warns ++ moreWarns) (x : xs')
  Err moreWarns env err -> Err (warns ++ moreWarns) env err
sequence (Err warns env err : _) = Err warns env err

----------------------------------------------------------------------
-- Warning Helpers
----------------------------------------------------------------------

-- | Add a warning to a Result
addWarning :: Warning -> Result a -> Result a
addWarning warn (Ok warns val) = Ok (warn : warns) val
addWarning warn (Err warns env err) = Err (warn : warns) env err

-- | Clear all warnings from a Result
clearWarnings :: Result a -> Result a
clearWarnings (Ok _ val) = Ok [] val
clearWarnings (Err _ env err) = Err [] env err

----------------------------------------------------------------------
-- Utility Functions
----------------------------------------------------------------------

-- | Convert an identifier to a string
identToString :: Abs.Ident -> String
identToString (Abs.Ident str) = str

-- | Extract the conclusion from a proof
getConclusion :: Proof -> Formula
getConclusion (Proof _ _ conc) = conc

-- | Convert a Result to a frontend-friendly error format
convertToFEError :: Result t -> FEResult
convertToFEError (Ok warns _) = FEOk (map convertWarning warns)
convertToFEError (Err warns env err) =
  let 
    baseMsg = errMessage err ++ 
              maybe "" (\ctx -> ": " ++ ctx) (errContext err)
    
    suggestionText = case errSuggestions err of
      [] -> ""
      [s] -> " Suggestion: " ++ s
      ss -> " Suggestions: " ++ List.intercalate "; " ss
      
    errorText = baseMsg ++ suggestionText
  in
    FEError (map convertWarning warns) (FELocal (getErrorLine env) errorText)

-- | Convert a warning to a frontend-friendly format
convertWarning :: Warning -> FEErrorWhere
convertWarning w = FELocal (fromMaybe (RefLine (-1)) (warnLocation w)) (show w)

-- | Get the current line number from the environment
getErrorLine :: Env -> Ref
getErrorLine env = fromMaybe (RefLine (-1)) (maybeHead (pos env))

-- | Get the first element of a list safely
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (h:_) = Just h