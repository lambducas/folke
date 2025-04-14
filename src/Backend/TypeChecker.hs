module Backend.TypeChecker (
    isProofCorrect,
    check,
    checkString,
    handleFrontendMessage,
) where

import qualified Logic.Abs as Abs
import Logic.Par (pSequent, myLexer)
import Shared.Messages
import Backend.Environment
import Backend.Types
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Backend.Helpers

{-
    Runs the parser and then the typechecker on a given string
    -params:
        - Code to check
    -return: Ok/Error
-}

-- | Make support for special characters

checkString :: String -> Result ()
checkString proof = do
    case pSequent (myLexer proof) of
        Left err -> Error [] newEnv (SyntaxError err)
        Right seq_t -> check seq_t
{-
    Typechecks a given proof and if it matches the sequent
    -params:
        - Sequent to check
    -return: Ok/Error
-}
check :: Abs.Sequent -> Result ()
check seq_t = do
    let env = newEnv
    seq_result <- checkSequent env seq_t
    return ()
{-
    Typechecks a given proof and if it matches the sequent and returns what it proves
    -params:
        - environment
        - Sequent to check 
    -return: The type of the proof
-}
checkSequent :: Env -> Abs.Sequent -> Result Proof
checkSequent _ (Abs.Seq _ Abs.FormNil (Abs.Proof _)) =
    Error [] newEnv (TypeError "Conclusion is empty.")
checkSequent env (Abs.Seq prems conc (Abs.Proof proof)) = do
    prems_t <- checkPrems env prems
    conc_t <- checkForm env conc
    proof_t <- checkProof env proof

    let endsWithEmptyLine = hasNilConclusion proof_t
    -- Filter out Nil conclusion from the proof
    let filteredProof = filterNilConclusion proof_t
    let seq_t = Proof [] prems_t conc_t

    if filteredProof == seq_t
        then return seq_t
        else if endsWithEmptyLine && not (hasInvalidConclusion filteredProof seq_t)
            
            then Ok [Warning env "Unfinished proof. The last line doesn't match the expected conclusion."] filteredProof
            else Error [] env (TypeError ("The proof " ++ show filteredProof ++ " did not match the expected " ++ show seq_t ++ "."))


checkPrems :: Env -> [Abs.Form] -> Result [Formula]
checkPrems _ [] = Ok [] []
checkPrems _ [Abs.FormNil] = Ok [] []
checkPrems env (form:forms) = do
    form_t <- checkForm env form
    forms_t <- checkPrems env forms
    Ok [] (form_t : forms_t)
{-
    Typechecks a given proof
    -params:
        - environment
        - List of steps in the proof
    -return: The type of the proof
-}
checkProof :: Env -> [Abs.ProofElem] -> Result Proof
checkProof env [] = Ok [] (Proof [] (getPrems env) Nil)
checkProof env [Abs.ProofElem labels step] =
    case step of
        Abs.StepNil -> Ok [] (Proof (getFreshs env) (getPrems env) Nil)
        _ -> do
            refs_t <- checkRefs labels
            (new_env, step_t) <- checkStep (pushPos env refs_t) step
            case step_t of
                ArgProof _ -> Error [] env (TypeError "Last step in proof was another proof.")
                ArgTerm _ -> Error [] env (TypeError "Check step could not return a term.")
                ArgFormWith _ _ -> Error [] env (TypeError "Check step could not return a form with.")
                ArgForm step_t -> Ok [] (Proof (getFreshs new_env) (getPrems new_env) step_t)
checkProof env (Abs.ProofElem labels step : elems) =
    case step of
        Abs.StepNil ->
            let warning = Warning env "Empty line in proof"
            in case checkProof env elems of
                Ok warns proof -> Ok (warning : warns) proof
                Error warns env_e err -> Error (warning : warns) env_e err
        _ -> do
            refs_t <- checkRefs labels
            (new_env, step_t) <- checkStep (pushPos env refs_t) step
            case elems of
                [Abs.ProofElem _ Abs.StepNil] ->
                    case step_t of
                        ArgForm conclusion ->
                            Ok [] (Proof (getFreshs new_env) (getPrems new_env) conclusion)
                        _ -> checkProof (popPos (addRefs new_env refs_t step_t) (toInteger $ List.length refs_t)) elems
                _ -> checkProof (popPos (addRefs new_env refs_t step_t) (toInteger $ List.length refs_t)) elems

-- Add back the checkRefs function
checkRefs :: [Abs.Label] -> Result [Ref]
checkRefs [] = Ok [] []
checkRefs (label:labels) = do
    refs_t <- checkRefs labels
    case label of
        Abs.LabelRange i j -> Ok [] (RefRange i j : refs_t)
        Abs.LabelLine i -> Ok [] (RefLine i : refs_t)

{-
    Typechecks a step in a proof, returns the environment becuase we do not change scope between each step.
    -params:
        - Environment
        - Step to check 
    -return: (Updated environment, type of the step)
-}
checkStep :: Env -> Abs.Step -> Result (Env, Arg)
checkStep env step = case step of
    Abs.StepPrem form -> if depth env == 0 then do
            form_t <- checkForm env form
            new_env <- addPrem env form_t
            Ok [] (new_env, ArgForm form_t)
        else Error [] env (UnknownError "A premise is not allowed in a subproof.")
    Abs.StepFresh ident -> do
        let t = Term (identToString ident) []
        env1 <- regTerm env t
        env2 <- addFresh env1 t
        Ok [] (env2, ArgTerm t)
    Abs.StepAssume form -> if depth env /= 0 then do
            form_t <- checkForm env form
            new_env <- addPrem env form_t
            Ok [] (new_env, ArgForm form_t)
        else Error [] env (UnknownError "An assumption is not allowed in a proof.")
    Abs.StepProof (Abs.Proof steps) ->
        -- Check if the subproof ends with an empty line
        case steps of
            [] -> Ok [] (env, ArgForm Nil)  -- Empty subproof
            [Abs.ProofElem _ Abs.StepNil] -> Ok [] (env, ArgForm Nil)  -- Subproof with just an empty line
            _ ->
                let lastStep = last steps
                in case lastStep of
                    -- If the last step is empty, treat as ArgForm Nil to avoid "another proof" error
                    Abs.ProofElem _ Abs.StepNil -> do
                        -- Process everything except the last empty line
                        proof_t <- checkProof (push env) (init steps)
                        -- Return the proof without the empty line's effect
                        Ok [] (env, ArgForm (getConclusion proof_t))
                    -- Normal case: process the whole subproof
                    _ -> do
                        proof_t <- checkProof (push env) steps
                        Ok [] (env, ArgProof proof_t)
    Abs.StepForm name args form -> do
        form_t <- checkForm env form
        (env1, args_t) <- checkArgs env args
        res_t <- applyRule env1 (identToString name) args_t form_t
        Ok [] (env1, ArgForm res_t)
    Abs.StepNil -> Error [] env (UnknownError "Unexpected StepNil")

checkArgs :: Env -> [Abs.Arg] -> Result (Env, [Arg])
checkArgs env [] = Ok [] (env, [])
checkArgs env (arg:args) = do
    (env1, args_t) <- checkArgs env args
    (env2, arg_t) <- checkArg env1 arg
    Ok [] (env2, arg_t : args_t)

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
checkArg env (Abs.ArgForm _ _) = Error [] env (TypeError "A formula argument cannot be over a function.")

checkTerm :: Env -> Abs.Term -> Result Term
checkTerm env (Abs.Term ident (Abs.Params terms)) = do
    let name = identToString ident
    case Map.lookup name (ids env) of
        Nothing -> do
            terms_t <- checkTerms env terms
            Ok [] (Term name terms_t)
        Just (IDTypePred _) -> Error [] env (UnknownError (name ++ " is a predicate."))
        Just (IDTypeTerm n) -> do
            let i = toInteger (length terms)
            if n /= i
                then Error [] env (UnknownError (name ++ " is arity " ++ show n ++ " not " ++ show i ++ "."))
                else do
                    terms_t <- checkTerms env terms
                    Ok [] (Term name terms_t)

checkTerms :: Env -> [Abs.Term] -> Result [Term]
checkTerms _ [] = Ok [] []
checkTerms env (x:xs) = do
    term_t <- checkTerm env x
    terms_t <- checkTerms env xs
    Ok [] (term_t : terms_t)

checkForm :: Env -> Abs.Form -> Result Formula
checkForm env f = case f of
    Abs.FormPar form -> checkForm env form
    Abs.FormBot -> Ok [] Bot
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
    Abs.FormNil -> Error [] env (TypeError "Formula is empty.")

checkPred :: Env -> Abs.Pred -> Result (Env, Predicate)
checkPred env (Abs.Pred ident (Abs.Params terms)) = do
    let name = identToString ident
    case Map.lookup name (ids env) of
        Nothing -> do
            terms_t <- checkTerms env terms
            Ok [] (env, Predicate name terms_t)
        Just (IDTypeTerm _) -> Error [] env (UnknownError (name ++ " is a term."))
        Just (IDTypePred n) -> do
            let i = toInteger (length terms)
            if n /= i
                then Error [] env (UnknownError (name ++ " is arity " ++ show n ++ " not " ++ show i ++ "."))
                else do
                    terms_t <- checkTerms env terms
                    Ok [] (env, Predicate name terms_t)


{-
    Typechecks if a given proof is correct and if it matches the sequent. Discards any error information
    -params:
        - Sequent to check
    -return: Yes/No
-}
isProofCorrect :: Abs.Sequent -> Bool
isProofCorrect seq_t = case check seq_t of
    Error {} ->  False
    Ok _ _ -> True

{-
    Allow frontend to check sequents in background-thread
-}

handleFrontendMessage :: FrontendMessage -> BackendMessage
handleFrontendMessage (CheckStringSequent text) = StringSequentChecked (convertToFEError (checkString text))
handleFrontendMessage (CheckSequent sequent) = SequentChecked (convertToFEError (check sequent))
handleFrontendMessage (CheckStep _) = StepChecked (Left "handleFrontendMessage: CheckStep not implemented")
handleFrontendMessage (OtherFrontendMessage text) = OtherBackendMessage text
