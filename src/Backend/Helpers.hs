module Backend.Helpers where

import qualified Logic.Abs as Abs
import Shared.Messages
import Backend.Environment
import Backend.Types 
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

hasNilConclusion :: Proof -> Bool
hasNilConclusion (Proof _ _ Nil) = True
hasNilConclusion _ = False

hasInvalidConclusion :: Proof -> Proof -> Bool
hasInvalidConclusion (Proof _ _ actual) (Proof _ _ expected) =
    actual /= expected && actual /= Nil && isSameType actual expected
    where
        -- Check if two formulas have the same type structure (e.g. both are implications, both are predicates, etc.)
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

-- Filter Nil conclusions from the proof result
filterNilConclusion :: Proof -> Proof
filterNilConclusion (Proof terms prems Nil) = 
    -- If there are any premises, use the last one as the conclusion
    case reverse prems of
        [] -> Proof terms prems Nil  -- No premises, keep Nil
        (lastPrem:rest) -> Proof terms (reverse rest) lastPrem  -- Use last premise as conclusion
filterNilConclusion proof = proof  -- Keep non-Nil conclusions as is


-- Returns warnings for unused references.
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
              else Ok [Warning env ("Unused references: " ++ unusedRefsStr)] ()
  where
    isProofReference (ArgProof _) = True  -- Don't count ArgProofs
    isProofReference _ = False

{-
    Converts identifier to string
    -params:
        - the Abs identifier 
    -return: string
-}
identToString :: Abs.Ident -> String
identToString (Abs.Ident str) = str

-- Helper function to extract the conclusion from a proof
getConclusion :: Proof -> Formula
getConclusion (Proof _ _ conc) = conc

-- Sending `Result` directly to frontend freezes the program
-- (because of env?) so we use env to calculate line numbers
-- first and then send back error without env in it
convertToFEError :: Result t -> FEResult
convertToFEError (Ok warns _) = FEOk (map convertWarning warns)
convertToFEError (Error warns env errorKind) = FEError (map convertWarning warns) (FELocal (getErrorLine env) (show errorKind))

convertWarning :: Warning -> FEErrorWhere
convertWarning (Warning env msg) = FELocal (getErrorLine env) msg

getErrorLine :: Env -> Ref
getErrorLine env = fromMaybe (RefLine (-1)) (maybeHead (pos env))

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (h:_) = Just h