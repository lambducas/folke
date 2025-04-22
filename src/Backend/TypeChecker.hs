{- |
Module      : Backend.TypeChecker
Description : Type checking and verification for logical proofs
Copyright   : (c) Your Organization, 2023
License     : GPL-3

This module provides functions to verify the correctness of logical proofs.
It handles both the syntactic correctness (type checking) and semantic correctness
(proof verification).
-}
module Backend.TypeChecker (
    -- * Main API functions
    checkJson,
    handleFrontendMessage,
) where

import qualified Logic.Abs as Abs
import Logic.Par (myLexer, pForm, pListArg)
import Shared.Messages
import Shared.FESequent as FE
import Backend.Environment
import Backend.Types
import Backend.Helpers

import Frontend.Parse (parseProofFromJSON)
import qualified Data.List as List
import qualified Data.Map as Map

import Data.Maybe (listToMaybe)
import Control.Exception (SomeException, try)
import System.IO.Unsafe (unsafePerformIO)

import Frontend.SpecialCharacters (replaceSpecialSymbolsInverse)
import Data.Text (Text, unpack, pack, intercalate)

----------------------------------------------------------------------
-- Main API Functions
----------------------------------------------------------------------
-- | Check a proof from a JSON file
checkJson :: FilePath -> Result ()
checkJson filePath =  do
    -- Read the file content
    fileContent <- case unsafePerformIO (try (readFile filePath) :: IO (Either SomeException String)) of
        Left err -> Err [] newEnv (createSyntaxError newEnv $ "Error reading file: " ++ show err)
        Right content -> Ok [] content
    
    -- Process the JSON content
    jsonText <- Ok [] (pack fileContent)
    seq <- case parseProofFromJSON jsonText of
        Nothing -> Err [] newEnv (createSyntaxError newEnv "Failed to parse JSON proof")
        Just s -> Ok [] s
    
    -- Check the proof with the backend
    checkFE seq

-- | Handle a message from the frontend
handleFrontendMessage :: FrontendMessage -> BackendMessage
handleFrontendMessage (CheckStringSequent _) =
    StepChecked (Left "handleFrontendMessage: No longer supports non JSON proofs")
handleFrontendMessage (CheckSequent _) =
    StepChecked (Left "handleFrontendMessage: No longer supports non JSON proofs")
handleFrontendMessage (CheckStep _) =
    StepChecked (Left "handleFrontendMessage: CheckStep not implemented")
handleFrontendMessage (OtherFrontendMessage text) =
    OtherBackendMessage text
handleFrontendMessage (CheckFESequent tree) = 
    StringSequentChecked (convertToFEError (checkFE tree))

----------------------------------------------------------------------
-- Frontend Sequent Checking
----------------------------------------------------------------------

-- | Check a FE (frontend) sequent
checkFE :: FE.FESequent -> Result ()
checkFE seq_t = do
    let env = newEnv
    _ <- checkSequentFE env seq_t
    return ()

-- | Check a frontend sequent against the environment
-- | Check a frontend sequent against the environment
checkSequentFE :: Env -> FE.FESequent -> Result Proof
checkSequentFE env sequent = do
    -- Convert premises to steps and combine with existing steps
    let steps = map premToStep (_premises sequent) ++ _steps sequent
    
    -- Check premises and conclusion
    prems_t <- checkPremsFE env (_premises sequent)
    conc_t <- checkFormFE env (_conclusion sequent) 
    
    -- Check the proof steps
    proof_t <- checkProofFE env steps 1
    
    -- Validate references
    validateRefs env

    -- Handle empty lines and check if proof is complete
    let endsWithEmptyLine = hasNilConclusion proof_t
    let filteredProof = filterNilConclusion proof_t
    let seq_t = Proof [] prems_t conc_t

    if filteredProof == seq_t
        then return seq_t
        else if endsWithEmptyLine  -- Simplified condition - only check for empty line
            then Ok [createIncompleteWarning env seq_t filteredProof] filteredProof
            else Err [] env (createMismatchedFormulaError env
                              (getConclusion seq_t)
                              (getConclusion filteredProof))
  where
    -- Create a warning for incomplete proofs
    createIncompleteWarning env seq_t filteredProof = Warning {
        warnLocation = listToMaybe (pos env),
        warnSeverity = Medium,
        warnKind = IncompleteProof (getConclusion seq_t) (getConclusion filteredProof),
        warnMessage = "Unfinished proof. The last line doesn't match the expected conclusion.",
        warnSuggestion = Just "Complete your proof to derive the required conclusion"
    }

-- | Convert a premise to a proof step for verification
premToStep :: FE.FEFormula -> FE.FEStep
premToStep form = Line {
    _statement = form,
    _rule = pack "prem",
    _usedArguments = 0,
    _arguments = []
}

-- | Check frontend formulas in premises
checkPremsFE :: Env -> [FE.FEFormula] -> Result [Formula]
checkPremsFE _ [] = Ok [] []
checkPremsFE env (form:forms) = do
    form_t <- checkFormFE env form
    forms_t <- checkPremsFE env forms
    Ok [] (form_t : forms_t)

-- | Check a frontend formula
checkFormFE :: Env -> FE.FEFormula -> Result Formula
checkFormFE env fef = do 
    f <- parseFormula fef
    checkForm env f

-- | Parse a frontend formula to an abstract syntax formula
parseFormula :: FE.FEFormula -> Result Abs.Form
parseFormula t =
    case pForm $ myLexer $ unpack $ replaceSpecialSymbolsInverse t of
        Left err -> throwTypeError newEnv $ 
            "Failed to parse formula: '" ++ unpack t ++ "'\nError: " ++ err
        Right form -> Ok [] form

----------------------------------------------------------------------
-- Frontend Proof Checking
----------------------------------------------------------------------

-- | Check a proof represented as frontend steps
checkProofFE :: Env -> [FE.FEStep] -> Integer -> Result Proof
checkProofFE env [] _ = Ok [] (Proof [] (getPrems env) Nil)
checkProofFE env [step] i
  | FE.SubProof _ <- step = Err [] env (createTypeError env "Last step in proof was another proof.")
  | FE.Line {} <- step = do
      (new_env, step_t) <- checkStepFE (pushPos env [RefLine i]) step
      case step_t of
        ArgTerm _ -> Err [] env (createTypeError env "Check step could not return a term.")
        ArgFormWith _ _ -> Err [] env (createTypeError env "Check step could not return a form with.")
        ArgForm step_t -> Ok [] (Proof (getFreshs new_env) (getPrems new_env) step_t)
checkProofFE env (step : elems) i
  | FE.SubProof steps <- step = do
      -- Handle subproof: create reference for the entire subproof range
      let refs_t = [RefRange i (i - 1 + countSteps (FE.SubProof steps))]
      proof_t <- checkProofFE (push (pushPos env refs_t)) steps i
      let step_result = ArgProof proof_t
      -- Continue checking the remaining elements with updated position
      seq_t <- checkProofFE (popPos (addRefs (pushPos env refs_t) refs_t step_result) 1) 
                           elems (i + countSteps (FE.SubProof steps))
      Ok [] seq_t

  | FE.Line {} <- step = do
      -- Handle line: create reference for this single line
      let refs_t = [RefLine i]
      (new_env, step_t) <- checkStepFE (pushPos env refs_t) step
      -- Continue checking the remaining elements
      seq_t <- checkProofFE (popPos (addRefs new_env refs_t step_t) (toInteger $ List.length refs_t)) 
                           elems (i + 1)
      Ok [] seq_t

-- | Count the number of steps in a proof (for reference numbering)
countSteps :: FEStep -> Integer
countSteps (Line _ _ _ _) = 1
countSteps (SubProof steps) = sum (map countSteps steps)

-- | Check a single frontend proof step
checkStepFE :: Env -> FE.FEStep -> Result (Env, Arg)
checkStepFE env step = case step of
    SubProof steps -> do
        proof_t <- checkProofFE (push env) steps 0
        Ok [] (env, ArgProof proof_t)
    Line form rule numofargs args -> do
        case unpack rule of 
            -- Handle premises
            "prem" -> if depth env == 0
                then do
                    form_t <- checkFormFE env form
                    new_env <- addPrem env form_t
                    Ok [] (new_env, ArgForm form_t)
                else Err [] env (createTypeError env "A premise is not allowed in a subproof.")
            
            -- Handle fresh variables
            "fresh" -> do
                let t = Term (unpack $ replaceSpecialSymbolsInverse form) []
                env1 <- regTerm env t
                env2 <- addFresh env1 t
                Ok [] (env2, ArgTerm t)
            
            -- Handle assumptions (for subproofs)
            "assume" -> if depth env /= 0
                then do
                    form_t <- checkFormFE env form
                    new_env <- addPrem env form_t
                    Ok [] (new_env, ArgForm form_t)
                else Err [] env (createTypeError env "An assumption is not allowed in a proof.")
            
            -- Handle general rule applications
            _ -> do
                form_t <- checkFormFE env form
                (env1, args_t) <- checkArgsFE env (take numofargs args)
                res_t <- applyRule env1 (unpack rule) args_t form_t
                Ok [] (env1, ArgForm res_t)

-- | Check frontend arguments against rules
checkArgsFE :: Env -> [Text] -> Result (Env, [Arg])
checkArgsFE env a = do
    args <- parseArgs a
    checkArgs env args

-- | Parse arguments from frontend text format
parseArgs :: [Text] -> Result [Abs.Arg]
parseArgs t = 
    let argText = unpack (intercalate (pack ",") (map replaceSpecialSymbolsInverse t))
    in case pListArg (myLexer argText) of
        Left err -> throwTypeError newEnv $ 
            "Could not parse argument list: '" ++ argText ++ "'\nError: " ++ err
        Right arg -> Ok [] arg

----------------------------------------------------------------------
-- BNFC Argument and Formula Checking
----------------------------------------------------------------------

-- | Check a list of arguments
checkArgs :: Env -> [Abs.Arg] -> Result (Env, [Arg])
checkArgs env [] = Ok [] (env, [])
checkArgs env (arg:args) = do
    (env1, args_t) <- checkArgs env args
    (env2, arg_t) <- checkArg env1 arg
    Ok [] (env2, arg_t : args_t)

-- | Check a single argument
checkArg :: Env -> Abs.Arg -> Result (Env, Arg)
checkArg env (Abs.ArgRange i j) = getRef env (RefRange i j)
checkArg env (Abs.ArgLine i) = getRef env (RefLine i)
checkArg env (Abs.ArgTerm term) = do
    term_t <- checkTerm env term
    Ok [] (env, ArgTerm term_t)
checkArg env (Abs.ArgForm (Abs.Term x (Abs.Params [])) form) = do
    let term_t = Term (identToString x) []
    new_env <- regTerm env term_t
    form_t <- checkForm new_env form
    Ok [] (new_env, ArgFormWith term_t form_t)
checkArg env (Abs.ArgForm _ _) =
    Err [] env (createTypeError env "A formula cannot be over a function.")

-- | Check a formula
checkForm :: Env -> Abs.Form -> Result Formula
checkForm env f = case f of
    -- Handle parenthesized formulas
    Abs.FormPar form ->
        checkForm env form

    -- Handle bottom (false)
    Abs.FormBot ->
        Ok [] Bot

    -- Handle equality
    Abs.FormEq a b -> do
        a_t <- checkTerm env a
        b_t <- checkTerm env b
        Ok [] (Eq a_t b_t)

    -- Handle predicates
    Abs.FormPred p -> do
        (_, p_t) <- checkPred env p
        Ok [] (Pred p_t)

    -- Handle universal quantification
    Abs.FormAll ident form -> do
        let x = Term (identToString ident) []
        env1 <- regTerm env x
        env2 <- bindVar env1 x
        a_t <- checkForm env2 form
        Ok [] (All x a_t)

    -- Handle existential quantification
    Abs.FormSome ident form -> do
        let x = Term (identToString ident) []
        env1 <- regTerm env x
        env2 <- bindVar env1 x
        a_t <- checkForm env2 form
        Ok [] (Some x a_t)

    -- Handle negation
    Abs.FormNot form -> do
        a_t <- checkForm env form
        Ok [] (Not a_t)

    -- Handle conjunction
    Abs.FormAnd left right -> do
        left_t <- checkForm env left
        right_t <- checkForm env right
        Ok [] (And left_t right_t)

    -- Handle disjunction
    Abs.FormOr left right -> do
        left_t <- checkForm env left
        right_t <- checkForm env right
        Ok [] (Or left_t right_t)

    -- Handle implication
    Abs.FormImpl left right -> do
        left_t <- checkForm env left
        right_t <- checkForm env right
        Ok [] (Impl left_t right_t)

    -- Handle empty formulas
    Abs.FormNil ->
        Err [] env (createTypeError env "Formula is empty.")

-- | Check a term
checkTerm :: Env -> Abs.Term -> Result Term
checkTerm env (Abs.Term ident (Abs.Params terms)) = do
    let name = identToString ident
    case Map.lookup name (ids env) of
        Nothing -> do
            terms_t <- checkTerms env terms
            Ok [] (Term name terms_t)
        Just (IDTypePred _) ->
            Err [] env (createTypeError env (name ++ " is a predicate."))
        Just (IDTypeTerm n) -> do
            let i = toInteger (length terms)
            if n /= i
                then Err [] env (createTypeError env (name ++
                      " is arity " ++ show n ++ " not " ++ show i ++ "."))
                else do
                    terms_t <- checkTerms env terms
                    Ok [] (Term name terms_t)

-- | Check a list of terms
checkTerms :: Env -> [Abs.Term] -> Result [Term]
checkTerms _ [] = Ok [] []
checkTerms env (x:xs) = do
    term_t <- checkTerm env x
    terms_t <- checkTerms env xs
    Ok [] (term_t : terms_t)

-- | Check a predicate
checkPred :: Env -> Abs.Pred -> Result (Env, Predicate)
checkPred env (Abs.Pred ident (Abs.Params terms)) = do
    let name = identToString ident
    case Map.lookup name (ids env) of
        Nothing -> do
            terms_t <- checkTerms env terms
            Ok [] (env, Predicate name terms_t)

        Just (IDTypeTerm _) ->
            Err [] env (createTypeError env (name ++ " is a term."))

        Just (IDTypePred n) -> do
            let i = toInteger (length terms)
            if n /= i
                then Err [] env (createTypeError env (name ++
                      " is arity " ++ show n ++ " not " ++ show i ++ "."))
                else do
                    terms_t <- checkTerms env terms
                    Ok [] (env, Predicate name terms_t)

