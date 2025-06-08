{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Backend.Helpers
  (
    ---------------------------------------
    -- Error and Warning handling helpers
    ---------------------------------------
    throwError,
    addWarning,
    clearWarnings,

    ---------------------------------------
    -- Proof verification helpers
    ---------------------------------------
    sendWarns,

    ---------------------------------------
    -- Utility functions
    ---------------------------------------
    idToString,
    getConclusion,
    convertToFEError,
    convertWarning,
    sequentSteps,
    countSteps,
    findLastFormula,
    premToStep,
    filterWarningsBySeverity,
    filterResultWarnings,
    severity

  ) where

import Data.Text hiding (length, map, null)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Logic.Abs as Abs
import Control.Monad (unless, when)

import Backend.Environment
import Shared.Messages
import Shared.FESequent as FE

----------------------------------------------------------------------
-- Error and Warning Helpers
----------------------------------------------------------------------

-- | Throw an error with the given error constructor and message
throwError :: (Env -> String -> Error) -> Env -> String -> Result a
throwError errCons env msg = Err [] env (errCons env msg)

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

validateRefs :: Env -> Result ()
validateRefs env =
    let allRefs = Map.toList (refs env)
        premises = getPrems env

        isSkippable ref arg = case arg of
                ArgProof _ -> True
                ArgForm form -> form == findLastFormula env || isPremiseRef ref premises
                _ -> False

        unusedRefs = [(ref, arg) | (ref, (count, arg)) <- allRefs,
                     count == 0 && not (isSkippable ref arg)]
    in
    (unless (null unusedRefs) $ Ok [createUnusedRefsWarning unusedRefs] ())

-- | Create warning for duplicate lines
validateDups :: Env -> Result ()
validateDups env =
  Control.Monad.when (dups (getPrems env)) $ Ok [createDupWarning env] ()

-- | Top-level warning creator
sendWarns :: Env -> Result ()
sendWarns env = validateDups env
                -- TODO: fix >> validateRefs env

filterWarningsBySeverity :: Severity -> [Warning] -> [Warning]
filterWarningsBySeverity minSeverity = List.filter (\w -> severityValue (warnSeverity w) >= severityValue minSeverity)

-- | Get numeric value of severity
severityValue :: Severity -> Int
severityValue Low = 1
severityValue Medium = 2
severityValue High = 3

-- | Filter warnings in a Result to only include high severity ones
filterResultWarnings :: Result a -> ([Warning] -> [Warning]) -> Result a
filterResultWarnings (Ok warnings value) f = Ok (f warnings) value
filterResultWarnings (Err warnings env err) f = Err (f warnings) env err

-- | Count steps for a sequent
sequentSteps :: FE.FESequent -> [FEStep]
sequentSteps s = map premToStep (_premises s) ++ _steps s

-- | Count the number of steps in a subproof (for reference numbering)
countSteps :: FEStep -> Integer
countSteps (Line {}) = 1
countSteps (SubProof steps) = sum $ map countSteps steps

-- | Check if a reference points to a premise
isPremiseRef :: Ref -> [Formula] -> Bool
isPremiseRef (RefLine n) premises =
            n <= toInteger (length premises) && n > 0
isPremiseRef _ _ = False

-- | Check if there are duplicate formulas
dups :: [Formula] -> Bool
dups xs = List.length (List.nub xs) /= length xs

-- | Find the last formula in the environment
findLastFormula :: Env -> Formula
findLastFormula env =
    case Map.foldrWithKey findForm Nil (refs env) of
        Nil ->
            if not (null (getPrems env))
            then case List.last (getPrems env) of
                Nil -> Nil
                lastPrem -> lastPrem
            else Nil
        form -> form
    where
        findForm _ (_, ArgForm form) Nil = form
        findForm _ (_, ArgProof proof) Nil = getConclusion proof
        findForm _ _ acc = acc

-- | Convert a premise to a proof step for verification
premToStep :: FE.FEFormula -> FE.FEStep
premToStep form = Line {
    _statement = form,
    _rule = pack "prem",
    _usedArguments = 0,
    _arguments = []
}

-- | Convert an ID to a string
idToString :: Abs.Ident -> String
idToString (Abs.Ident str) = str

-- | Extract the conclusion from a proof
getConclusion :: Proof -> Formula
getConclusion (Proof _ _ conc) = conc

-- | Convert a Result to a frontend-friendly error format
convertToFEError :: Result t -> FEResult
convertToFEError (Ok warns _) = FEOk (map convertWarning warns)
convertToFEError (Err warns _env err) = feError
  where
    baseMsg = errMessage err ++
              maybe "" (\ctx -> ": " ++ ctx) (errContext err)

    suggestionText = case errSuggestions err of
      [] -> ""
      [s] -> "\nSuggestion: " ++ s
      ss -> "\nSuggestions: " ++ List.intercalate "; " ss

    errorText = baseMsg ++ suggestionText

    convWarns = map convertWarning warns
    feError = case errLocation err of
      Nothing -> FEError convWarns (FEGlobal errorText)
      Just l -> FEError convWarns (FELocal l errorText)

-- | Convert a warning to a frontend-friendly format
convertWarning :: Warning -> FEErrorWhere
convertWarning w = case warnLocation w of
  Nothing -> FEGlobal (show w)
  Just l -> FELocal l (show w)

severity :: Int -> Severity
severity i | i>=3=High
           | i<=1=Low
           | otherwise=Medium
