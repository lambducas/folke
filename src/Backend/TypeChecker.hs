{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Backend.TypeChecker (
    -- * Main API functions
    checkJson,
    checkFE,
    handleFrontendMessage,
    parseForm,
    parseArgs
) where

import qualified Logic.Abs as Abs
import Logic.Par (myLexer, pForm, pListArg)
import Shared.Messages
import Shared.FESequent as FE
import Backend.Environment
import Backend.Helpers

import Frontend.Parse (parseProofFromJSON)
import qualified Data.List as List
import qualified Data.Map as Map

import Data.Maybe (fromMaybe)
import Control.Exception (SomeException, try)
import System.IO.Unsafe (unsafePerformIO)

import Shared.SpecialCharacters (replaceSpecialSymbolsInverse)
import Data.Text (Text, unpack, pack, intercalate, strip)

----------------------------------------------------------------------
-- Main API Functions
----------------------------------------------------------------------

-- | Handle a message from the frontend
handleFrontendMessage :: FrontendMessage -> BackendMessage
handleFrontendMessage (OtherFrontendMessage text) =
    OtherBackendMessage text
handleFrontendMessage (CheckFEDocument (doc, (userSensitivity, acpActive))) =
    FEDocumentChecked backendMessage
       where backendMessage = 
              if acpActive -- if autocheck proof is enabled
              then convertToFEError $
              filterResultWarnings (checkFE doc) (filterWarningsBySeverity (severity userSensitivity)) -- filter logic
              else convertToFEError $ checkFE doc

-- | Check a proof from a JSON file
checkJson :: FilePath -> Result ()
checkJson filePath =  do
    -- Read the file content
    fileContent <- case unsafePerformIO (try (readFile filePath) :: IO (Either SomeException String)) of
        Left err -> Err [] newEnv (createSyntaxError newEnv $ "Error reading file: " ++ show err)
        Right content -> Ok [] content

    -- Process the JSON content
    jsonText <- Ok [] (pack fileContent)
    doc <- case parseProofFromJSON jsonText of
        Nothing -> Err [] newEnv (createSyntaxError newEnv "Failed to parse JSON proof")
        Just s -> Ok [] s


    -- Check the proof with the backend
    checkFE doc

----------------------------------------------------------------------
-- Frontend Sequent Checking
----------------------------------------------------------------------

-- | Check a FE (frontend) sequent
checkFE :: FE.FEDocument -> Result ()
checkFE doc = do
    -- Extract sequent from document
    let seq = _sequent doc

    rules <- checkFEUserDefinedRules newEnv (fromMaybe [] $ _fedUserDefinedRules doc)

    let env = newEnv { user_rules = rules}

    (_, finalEnv) <- checkSequentFE env seq
    sendWarns finalEnv
    return ()

checkFEUserDefinedRules :: Env -> [FE.FEUserDefinedRule] -> Result (Map.Map String UDefRule)
checkFEUserDefinedRules _ [] = Ok [] Map.empty
checkFEUserDefinedRules env [x] = checkFEUserDefinedRule env x
checkFEUserDefinedRules env (x:xs) = do
    rs <- checkFEUserDefinedRules env xs
    r  <- checkFEUserDefinedRule env  x
    Ok [] (Map.union r rs)

checkFEUserDefinedRule :: Env -> FE.FEUserDefinedRule -> Result (Map.Map String UDefRule)
checkFEUserDefinedRule env (FE.FEUserDefinedRule name path ins out) = do
    case (ins, out) of
        (Just ins, Just out) -> do
            ins_t <- checkPremsFE newEnv ins
            out_t <- checkFormFE newEnv out
            Ok [] (Map.singleton (unpack name) (UDefRule ins_t out_t))
        _ -> Err [] env (createUnknownError env ("Invalid user defined rule from " ++ path))

-- | Check a FE (frontend) sequent
checkSequentFE :: Env -> FE.FESequent -> Result (Proof, Env)
checkSequentFE env sequent = do
    -- Checking and validation
    prems_t <- checkPremsFE env (_premises sequent)
    conc_t <- checkConcFE env (_conclusion sequent)
    (proof_t, finalEnv) <- checkProofFE env (sequentSteps sequent) 1

    -- Check expected vs actual
    let seq_t = Proof [] prems_t conc_t
    if proof_t == seq_t
         then return (seq_t, finalEnv)
         else Err [] finalEnv (createMismatchedFormulaError finalEnv conc_t (getConclusion proof_t))

-- | Check frontend formulas in premises
checkPremsFE :: Env -> [FE.FEFormula] -> Result [Formula]
checkPremsFE _ [] = Ok [] []
checkPremsFE env forms = checkPremsFEWithIndex env forms 0
    where
        checkPremsFEWithIndex _ [] _ = Ok [] []
        checkPremsFEWithIndex env (form:forms) idx = do
            f <- parseForm env form
            form_t <- case f of
                Abs.FormNil -> Err [] env (createEmptyPremiseError env idx)
                _ -> checkForm env f

            forms_t <- checkPremsFEWithIndex env forms (idx + 1)
            Ok [] (form_t : forms_t)

-- | Check frontend conclusion
checkConcFE :: Env -> FE.FEFormula -> Result Formula
checkConcFE env f = do
    f <- parseForm env f
    case f of
        Abs.FormNil -> Err [] env (createEmptyConcError env)
        _ -> checkForm env f

-- | Check a frontend formula
checkFormFE :: Env -> FE.FEFormula -> Result Formula
checkFormFE env fef = do
    f <- parseForm env fef
    checkForm env f

parseForm :: Env -> Text -> Result Abs.Form
parseForm env t =
    case pForm $ myLexer $ unpack $ replaceSpecialSymbolsInverse t of
    Left err -> throwError createSyntaxError env $
        "Failed to parse formula: '" ++ unpack t ++ "'\nError: " ++ err
    Right form -> Ok [] form

----------------------------------------------------------------------
-- Frontend Proof Checking
----------------------------------------------------------------------

-- | Check a proof and return both the proof and the final environment
checkProofFE :: Env -> [FE.FEStep] -> Integer -> Result (Proof, Env)
checkProofFE env [] _ = Ok [] (Proof [] (getPrems env) Nil, env)

checkProofFE env [step] i
    | FE.SubProof steps <- step = do
            let refs_t = [RefRange i (i - 1 + countSteps (FE.SubProof steps))]
            let newEnv = push (pushPos env refs_t)
            _ <- checkProofFE newEnv steps i
            Err [] newEnv (createTypeError newEnv "Last step in proof was another proof.")
    | FE.Line {} <- step = do
        (new_env, step_t) <- checkStepFE (pushPos env [RefLine i]) step
        case step_t of
            ArgProof {} -> Err [] env (createTypeError env "Check step could not return a proof.")
            ArgTerm _ -> Err [] env (createTypeError env "Check step could not return a term.")
            ArgFormWith _ _ -> Err [] env (createTypeError env "Check step could not return a form with.")
            ArgForm Nil ->
                let lastForm = findLastFormula env
                in if lastForm /= Nil
                   then Ok [] (Proof (getFreshs new_env) (getPrems new_env) lastForm, new_env)
                   else Ok [] (Proof (getFreshs new_env) (getPrems new_env) Nil, new_env)
            ArgForm step_t -> Ok [] (Proof (getFreshs new_env) (getPrems new_env) step_t, new_env)

checkProofFE env (step : elems) i
    | FE.SubProof steps <- step = do
        let refs_t = [RefRange i (i - 1 + countSteps (FE.SubProof steps))]
        (proof_t, _) <- checkProofFE (push (pushPos env refs_t)) steps i
        let step_result = ArgProof proof_t
        let env' = popPos (addRefs (pushPos env refs_t) refs_t step_result) 1
        (seq_t, finalEnv) <- checkProofFE env' elems (i + countSteps (FE.SubProof steps))
        Ok [] (seq_t, finalEnv)
    | FE.Line {} <- step = do

        let refs_t = [RefLine i]
        (new_env, step_t) <- checkStepFE (pushPos env refs_t) step

        let env' = popPos (addRefs new_env refs_t step_t) (toInteger $ List.length refs_t)
        (seq_t, finalEnv) <- checkProofFE env' elems (i + 1)
        Ok [] (seq_t, finalEnv)

-- | Check a single frontend proof step
checkStepFE :: Env -> FE.FEStep -> Result (Env, Arg)
checkStepFE env step = case step of
    FE.SubProof steps -> do
        (proof_t, _) <- checkProofFE (push env) steps 0
        let currentRef = head (pos env)
        let env_with_ref = addRefs env [currentRef] (ArgProof proof_t)
        Ok [] (env_with_ref, ArgProof proof_t)
        
    FE.Line form rule numofargs args -> do
        let currentRef = head (pos env)
        let formIsEmpty = strip form == pack ""
        let ruleIsEmpty = strip rule == pack ""
        
        if formIsEmpty && ruleIsEmpty then 
            let env_with_ref = addRefs env [currentRef] (ArgForm Nil)
            in Ok [createEmptyLineWarning env_with_ref] (env_with_ref, ArgForm Nil)
            
        else if ruleIsEmpty then 
            Err [] env (createNoRuleProvidedError env)

        else if formIsEmpty then 
            Err [] env (createNoFormulaProvidedError env)
            
        else case unpack rule of
            "prem" -> if depth env == 0
                then do
                    form_t <- checkFormFE env form
                    new_env <- addPrem env form_t
                    let env_with_ref = addRefs new_env [currentRef] (ArgForm form_t)
                    Ok [] (env_with_ref, ArgForm form_t)
                else Err [] env (createTypeError env "A premise is not allowed in a subproof.")

            -- Handle fresh variables
            "fresh" -> do
                let t = Term (unpack $ replaceSpecialSymbolsInverse form) []
                env1 <- regTerm env t
                env2 <- addFresh env1 t
                -- Register the reference for this line
                let env_with_ref = addRefs env2 [currentRef] (ArgTerm t)
                Ok [] (env_with_ref, ArgTerm t)

            -- Handle assumptions (for subproofs)
            "assume" -> if depth env /= 0
                then do
                    form_t <- checkFormFE env form
                    new_env <- addPrem env form_t
                    -- Register the reference for this line
                    let env_with_ref = addRefs new_env [currentRef] (ArgForm form_t)
                    Ok [] (env_with_ref, ArgForm form_t)
                else Err [] env (createTypeError env "Assumptions are only allowed in subproofs.")

            -- Handle general rule applications
            _ -> do
                form_t <- checkFormFE env form
                (env1, args_t) <- checkArgsFE env (take numofargs args)
                res_t <- applyRule env1 (unpack rule) args_t form_t
                let env_with_ref = addRefs env1 [currentRef] (ArgForm res_t)
                Ok [] (env_with_ref, ArgForm res_t)

-- | Check frontend arguments against rules
checkArgsFE :: Env -> [Text] -> Result (Env, [Arg])
checkArgsFE env a = do
    args <- parseArgs env a
    checkArgs env args

-- | Parse arguments from frontend text format
parseArgs :: Env -> [Text] -> Result [Abs.Arg]
parseArgs env t =
    let argText = unpack (intercalate (pack ",") (map replaceSpecialSymbolsInverse t))
    in case pListArg (myLexer argText) of
        Left err -> throwError createSyntaxError env $
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
checkArg env (Abs.ArgForm (Abs.Term0 x ) form) = do
    let term_t = Term (idToString x) []
    new_env <- regTerm env term_t
    form_t <- checkForm new_env form
    Ok [] (new_env, ArgFormWith term_t form_t)
checkArg env (Abs.ArgForm _ _) =
    Err [] env (createTypeError env "A formula cannot be over a function.")
checkArg env Abs.ArgNil =
    Err [] env (createTypeError env "Argument cannot be empty")

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
    Abs.FormAll ident form -> checkAll ident form
    Abs.FormAllDot ident form -> checkAll ident form

    -- Handle existential quantification
    Abs.FormSome ident form -> checkSome ident form
    Abs.FormSomeDot ident form -> checkSome ident form

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
        return Nil

    where
        checkAll ident form = do
            let x = Term (idToString ident) []
            env1 <- regTerm env x
            env2 <- bindVar env1 x
            a_t <- checkForm env2 form
            Ok [] (All x a_t)

        checkSome ident form = do
            let x = Term (idToString ident) []
            env1 <- regTerm env x
            env2 <- bindVar env1 x
            a_t <- checkForm env2 form
            Ok [] (Some x a_t)


-- | Check a term
checkTerm :: Env -> Abs.Term -> Result Term
checkTerm env (Abs.TermN ident terms) = do
    let name = idToString ident
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
checkTerm env (Abs.Term0 ident) = do
    let name = idToString ident
    case Map.lookup name (ids env) of
        Nothing -> Ok [] (Term name [])
        Just (IDTypePred _) ->
            Err [] env (createTypeError env (name ++ " is a predicate."))
        Just (IDTypeTerm n) -> do
            if n /= 0
                then Err [] env (createTypeError env (name ++
                      " is arity " ++ show n ++ " not 0."))
                else Ok [] (Term name [])

-- | Check a list of terms
checkTerms :: Env -> [Abs.Term] -> Result [Term]
checkTerms _ [] = Ok [] []
checkTerms env (x:xs) = do
    term_t <- checkTerm env x
    terms_t <- checkTerms env xs
    Ok [] (term_t : terms_t)

-- | Check a predicate
checkPred :: Env -> Abs.Pred -> Result (Env, Predicate)
checkPred env (Abs.PredN ident terms) = do
    let name = idToString ident
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
checkPred env (Abs.Pred0 ident) = do
    let name = idToString ident
    case Map.lookup name (ids env) of
        Nothing -> Ok [] (env, Predicate name [])

        Just (IDTypeTerm _) ->
            Err [] env (createTypeError env (name ++ " is a term."))

        Just (IDTypePred n) -> do
            if n /= 0
                then Err [] env (createTypeError env (name ++
                      " is arity " ++ show n ++ " not 0."))
                else Ok [] (env, Predicate name [])
