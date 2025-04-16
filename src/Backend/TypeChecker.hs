module Backend.TypeChecker (
    -- * Main API functions
    isProofCorrect,
    check,
    checkString,
    checkJson,
    handleFrontendMessage,
) where

import qualified Logic.Abs as Abs
import Logic.Par (pSequent, myLexer)
import Shared.Messages
import Backend.Environment
import Backend.Types
import Backend.Helpers

import Frontend.Parse (parseProofFromJSON, parseProofForBackend)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Text (unpack, pack)
import Control.Exception (SomeException, try)
import System.IO.Unsafe (unsafePerformIO)

----------------------------------------------------------------------
-- Main API Functions
----------------------------------------------------------------------

-- | Check if a proof is correct (returns a simple boolean)
isProofCorrect :: Abs.Sequent -> Bool
isProofCorrect seq_t = case check seq_t of
    Err {} ->  False
    Ok _ _ -> True

-- | Check a proof from a string (for external interfaces)
checkString :: String -> Result ()
checkString proof =
    case pSequent (myLexer proof) of
        Left err -> Err [] newEnv (createSyntaxError newEnv err)
        Right seq_t -> check seq_t

-- | Check a proof from a JSON file
checkJson :: FilePath -> Result ()
checkJson filePath =  do
    fileContent <- case unsafePerformIO (try (readFile filePath) :: IO (Either SomeException String)) of
        Left err -> Err [] newEnv (createSyntaxError newEnv $ "Error reading file: " ++ show err)
        Right content -> Ok [] content
    
    -- Process the JSON content
    jsonText <- Ok [] (pack fileContent)
    seq <- case parseProofFromJSON jsonText of
        Nothing -> Err [] newEnv (createSyntaxError newEnv "Failed to parse JSON proof")
        Just s -> Ok [] s
    
    backendText <- case Just (unpack (parseProofForBackend seq)) of
        Nothing -> Err [] newEnv (createSyntaxError newEnv "Failed to convert proof to backend format")
        Just t -> Ok [] t
    
    -- Check the proof with the backend
    result <- checkString backendText
    
    -- Return the appropriate result
    return ()

-- | Handle a message from the frontend 
handleFrontendMessage :: FrontendMessage -> BackendMessage
handleFrontendMessage (CheckStringSequent text) =
    StringSequentChecked (convertToFEError (checkString text))
handleFrontendMessage (CheckSequent sequent) =
    SequentChecked (convertToFEError (check sequent))
handleFrontendMessage (CheckStep _) =
    StepChecked (Left "handleFrontendMessage: CheckStep not implemented")
handleFrontendMessage (OtherFrontendMessage text) =
    OtherBackendMessage text

-- | Main function to typecheck a sequent
check :: Abs.Sequent -> Result ()
check seq_t = do
    let env = newEnv
    _ <- checkSequent env seq_t
    return ()

----------------------------------------------------------------------
-- Sequent and Proof Checking
----------------------------------------------------------------------

-- | Check a full sequent and return the resulting proof
checkSequent :: Env -> Abs.Sequent -> Result Proof
checkSequent _ (Abs.Seq _ Abs.FormNil (Abs.Proof _)) =
    Err [] newEnv (createTypeError newEnv "Conclusion is empty.")
checkSequent env (Abs.Seq prems conc (Abs.Proof proof)) = do
    prems_t <- checkPrems env prems
    conc_t <- checkForm env conc
    proof_t <- checkProof env proof

    -- Check for unused references
    validateRefs env

    let endsWithEmptyLine = hasNilConclusion proof_t
    -- Filter out Nil conclusion from the proof
    let filteredProof = filterNilConclusion proof_t
    let seq_t = Proof [] prems_t conc_t

    if filteredProof == seq_t
        then return seq_t
        else if endsWithEmptyLine && not (hasInvalidConclusion filteredProof seq_t)
            then Ok [createIncompleteWarning env seq_t filteredProof] filteredProof
            else Err [] env (createMismatchedFormulaError env
                              (getConclusion seq_t)
                              (getConclusion filteredProof))
  where
    createIncompleteWarning env seq_t filteredProof = Warning {
        warnLocation = listToMaybe (pos env),
        warnSeverity = Medium,
        warnKind = IncompleteProof (getConclusion seq_t) (getConclusion filteredProof),
        warnMessage = "Unfinished proof. The last line doesn't match the expected conclusion.",
        warnSuggestion = Just "Complete your proof to derive the required conclusion"
    }

-- | Check premises of a sequent
checkPrems :: Env -> [Abs.Form] -> Result [Formula]
checkPrems _ [] = Ok [] []
checkPrems _ [Abs.FormNil] = Ok [] []
checkPrems env (form:forms) = do
    form_t <- checkForm env form
    forms_t <- checkPrems env forms
    Ok [] (form_t : forms_t)

-- | Check a proof and return its type
checkProof :: Env -> [Abs.ProofElem] -> Result Proof
checkProof env [] = Ok [] (Proof [] (getPrems env) Nil)
checkProof env [Abs.ProofElem labels step] =
    case step of
        Abs.StepNil -> Ok [] (Proof (getFreshs env) (getPrems env) Nil)
        _ -> do
            refs_t <- checkRefs labels
            (new_env, step_t) <- checkStep (pushPos env refs_t) step
            case step_t of
                ArgProof _ ->
                    Err [] env (createTypeError env "Last step in proof was another proof.")
                ArgTerm _ ->
                    Err [] env (createTypeError env "Check step could not return a term.")
                ArgFormWith _ _ ->
                    Err [] env (createTypeError env "Check step could not return a form with.")
                ArgForm step_t ->
                    Ok [] (Proof (getFreshs new_env) (getPrems new_env) step_t)

checkProof env (Abs.ProofElem labels step : elems) =
    case step of
        Abs.StepNil ->
            let warning = Warning {
                warnLocation = listToMaybe (pos env),
                warnSeverity = Low,
                warnKind = StyleIssue "Empty line",
                warnMessage = "Empty line in proof",
                warnSuggestion = Just "Consider removing unnecessary empty lines for clarity"
            }
            in case checkProof env elems of
                Ok warns proof -> Ok (warning : warns) proof
                Err warns env_e err -> Err (warning : warns) env_e err
        _ -> do
            refs_t <- checkRefs labels
            (new_env, step_t) <- checkStep (pushPos env refs_t) step
            case elems of
                [Abs.ProofElem _ Abs.StepNil] ->
                    case step_t of
                        ArgForm conclusion ->
                            Ok [] (Proof (getFreshs new_env) (getPrems new_env) conclusion)
                        _ -> checkProofContinuation new_env refs_t step_t elems
                _ -> checkProofContinuation new_env refs_t step_t elems
  where
    checkProofContinuation new_env refs_t step_t = checkProof (popPos (addRefs new_env refs_t step_t)
                   (toInteger $ List.length refs_t))

-- | Check references in a proof step
checkRefs :: [Abs.Label] -> Result [Ref]
checkRefs [] = Ok [] []
checkRefs (label:labels) = do
    refs_t <- checkRefs labels
    case label of
        Abs.LabelRange i j -> Ok [] (RefRange i j : refs_t)
        Abs.LabelLine i -> Ok [] (RefLine i : refs_t)

----------------------------------------------------------------------
-- Step Checking
----------------------------------------------------------------------

-- | Check a step in a proof
checkStep :: Env -> Abs.Step -> Result (Env, Arg)
checkStep env step = case step of
    Abs.StepPrem form ->
        if depth env == 0
            then do
                form_t <- checkForm env form
                new_env <- addPrem env form_t
                Ok [] (new_env, ArgForm form_t)
            else Err [] env (createTypeError env "A premise is not allowed in a subproof.")

    Abs.StepFresh ident -> do
        let t = Term (identToString ident) []
        env1 <- regTerm env t
        env2 <- addFresh env1 t
        Ok [] (env2, ArgTerm t)

    Abs.StepAssume form ->
        if depth env /= 0
            then do
                form_t <- checkForm env form
                new_env <- addPrem env form_t
                Ok [] (new_env, ArgForm form_t)
            else Err [] env (createTypeError env "An assumption is not allowed in a proof.")

    Abs.StepProof (Abs.Proof steps) ->
        case steps of
            [] -> Ok [] (env, ArgForm Nil)
            [Abs.ProofElem _ Abs.StepNil] -> Ok [] (env, ArgForm Nil)
            _ ->
                let lastStep = last steps
                in case lastStep of
                    Abs.ProofElem _ Abs.StepNil -> do
                        proof_t <- checkProof (push env) (init steps)
                        Ok [] (env, ArgForm (getConclusion proof_t))
                    _ -> do
                        proof_t <- checkProof (push env) steps
                        Ok [] (env, ArgProof proof_t)

    Abs.StepForm name args form -> do
        form_t <- checkForm env form
        (env1, args_t) <- checkArgs env args
        res_t <- applyRule env1 (identToString name) args_t form_t
        Ok [] (env1, ArgForm res_t)

    Abs.StepNil ->
        Err [] env (createUnknownError env "Unexpected empty line in proof")

----------------------------------------------------------------------
-- Argument Checking
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

----------------------------------------------------------------------
-- Term and Formula Checking
----------------------------------------------------------------------

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

-- | Check a formula
checkForm :: Env -> Abs.Form -> Result Formula
checkForm env f = case f of
    Abs.FormPar form ->
        checkForm env form

    Abs.FormBot ->
        Ok [] Bot

    Abs.FormEq a b -> do
        a_t <- checkTerm env a
        b_t <- checkTerm env b
        Ok [] (Eq a_t b_t)

    Abs.FormPred p -> do
        (_, p_t) <- checkPred env p
        Ok [] (Pred p_t)

    Abs.FormAll ident form -> do
        let x = Term (identToString ident) []
        env1 <- regTerm env x
        env2 <- bindVar env1 x
        a_t <- checkForm env2 form
        Ok [] (All x a_t)

    Abs.FormSome ident form -> do
        let x = Term (identToString ident) []
        env1 <- regTerm env x
        env2 <- bindVar env1 x
        a_t <- checkForm env2 form
        Ok [] (Some x a_t)

    Abs.FormNot form -> do
        a_t <- checkForm env form
        Ok [] (Not a_t)

    Abs.FormAnd left right -> do
        left_t <- checkForm env left
        right_t <- checkForm env right
        Ok [] (And left_t right_t)

    Abs.FormOr left right -> do
        left_t <- checkForm env left
        right_t <- checkForm env right
        Ok [] (Or left_t right_t)

    Abs.FormImpl left right -> do
        left_t <- checkForm env left
        right_t <- checkForm env right
        Ok [] (Impl left_t right_t)

    Abs.FormNil ->
        Err [] env (createTypeError env "Formula is empty.")

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
