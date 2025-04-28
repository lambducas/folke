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
    validateRefs,

    ---------------------------------------
    -- Utility functions
    ---------------------------------------
    identToString,
    getConclusion,
    convertToFEError,
    convertWarning,
    getErrorLine,
    maybeHead,
    sequentSteps,
    countSteps,
    findLastFormula,
    premToStep,
    identToString 
    
  ) where

import Data.Text hiding (map, null)
import Data.Maybe (fromMaybe)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Logic.Abs as Abs
import Debug.Trace (trace)

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

-- | TODO: Fix
validateRefs :: Env -> Result ()
validateRefs env =
    let allRefs = Map.toList (refs env)
        unusedRefs = [(ref, arg) |
                     (ref, (count, arg)) <- allRefs,
                     count == 0]
        unusedRefsStr = List.intercalate ", " [show ref ++ "(" ++ show count ++ ")" | 
                                              (ref, (count, _)) <- allRefs]
    in trace ("All refs: " ++ unusedRefsStr) $
       if not (null unusedRefs)
       then Ok [Warning {
                warnLocation = Nothing,
                warnSeverity = Low,
                warnKind = StyleIssue "Unused references",
                warnMessage = "Some references were defined but never used: " ++ 
                              List.intercalate ", " [show r | (r, _) <- unusedRefs],
                warnSuggestion = Just "Consider removing unused references for cleaner proofs"
            }] ()
       else return ()
         
{-          let unusedRefsStr = List.intercalate ", " [show ref | (ref, _) <- unusedRefs]
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
-}

-- | Count steps for a sequent
sequentSteps :: FE.FESequent -> [FEStep]
sequentSteps sequent = map premToStep (_premises sequent) ++ _steps sequent

-- | Count the number of steps in a subproof (for reference numbering)
countSteps :: FEStep -> Integer
countSteps (Line {}) = 1
countSteps (SubProof steps) = sum $ map countSteps steps

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
