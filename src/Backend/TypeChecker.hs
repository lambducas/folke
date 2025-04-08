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

import Data.Maybe (fromMaybe)

{-
    Runs the parser and then the typechecker on a given string
    -params:
        - Code to check
    -return: Ok/Error
-}

-- | Make support for special characters

checkString :: String -> Result ()
checkString proof = 
    case pSequent (myLexer proof) of
        Left err -> Error [] newEnv (SyntaxError err)
        Right seq_t -> check seq_t
{-
    Typechecks if a given proof is correct and if it matches the sequent. Discards any error information
    -params:
        - Sequent to check
    -return: Yes/No
-}
isProofCorrect :: Abs.Sequent -> Bool
isProofCorrect seq_t = case check seq_t of
    Error _ _ _ ->  False
    Ok _ _ -> True

{-
    Typechecks a given proof and if it matches the sequent
    -params:
        - Sequent to check
    -return: Ok/Error
-}
check :: Abs.Sequent -> Result ()
check seq_t = do 
    let env = newEnv
    case checkSequent env seq_t of
        Error warns err_env err -> Error warns err_env err
        Ok warns _ -> Ok warns ()
{-
    Typechecks a given proof and if it matches the sequent and returns what it proves
    -params:
        - environment
        - Sequent to check 
    -return: The type of the proof
-}
checkSequent :: Env -> Abs.Sequent -> Result Proof
checkSequent _ (Abs.Seq _ Abs.FormNil (Abs.Proof _)) = Error [] newEnv (TypeError "Conclusion is empty.")
checkSequent env (Abs.Seq prems conc (Abs.Proof proof)) = case checkPrems env prems of
    Error warns err_env err -> Error warns err_env err
    Ok warns1 prems_t -> case checkForm env conc of
        Error warns err_env err -> Error (warns ++ warns1) err_env err
        Ok warns2 conc_t -> case checkProof env proof of
            Error warns err_env err -> Error (warns ++ warns1 ++ warns2) err_env err
            Ok warns3 proof_t -> do
                let seq_t = Proof [] prems_t conc_t -- TODO special check for variables?
                if proof_t == seq_t
                    then Ok (warns1 ++ warns2 ++ warns3) seq_t
                    else Error (warns1 ++ warns2 ++ warns3) env (TypeError ("The proof " ++ show proof_t ++ " did not match the expected " ++ show seq_t ++ "."))

checkPrems :: Env -> [Abs.Form] -> Result [Formula]
checkPrems _ []             = Ok [] []
checkPrems _ [Abs.FormNil]  = Ok [] []
checkPrems env (form:forms) = case checkForm env form of
    Error warns err_env err -> Error warns err_env err
    Ok warns1 form_t      -> case checkPrems env forms of
        Error warns err_env err -> Error (warns++warns1) err_env err
        Ok warns2 forms_t     -> Ok (warns1++warns2) (form_t : forms_t)

{-
    Typechecks a given proof
    -params:
        - environment
        - List of steps in the proof
    -return: The type of the proof
-}
checkProof :: Env -> [Abs.ProofElem] -> Result Proof
checkProof env [] = Ok [] (Proof [] (getPrems env) Nil)
checkProof env [Abs.ProofElem labels step] = case checkRefs labels of
    Error warns err_env err -> Error warns err_env err
    Ok warns1 refs_t -> case checkStep (pushPos env refs_t) step of
        Error warns err_env err -> Error (warns++warns1) err_env err
        Ok warns2 (_, ArgProof _) -> Error (warns1++warns2) env (TypeError "Last step in proof was another proof.")
        Ok warns2 (_, ArgTerm _) -> Error (warns1++warns2) env (TypeError "Check step could not return an term.")
        Ok warns2 (_, ArgFormWith _ _) -> Error (warns1++warns2) env (TypeError "Check step could not return an form with.")
        Ok warns2 (new_env, ArgForm step_t) -> Ok (warns1++warns2) (Proof (getFrees new_env) (getPrems new_env) step_t)
checkProof env ((Abs.ProofElem labels step):elems) = case checkRefs labels of
    Error warns err_env err -> Error warns err_env err
    Ok warns1 refs_t -> case checkStep (pushPos env refs_t) step of 
        Error warns err_env err -> Error (warns++warns1) err_env err
        Ok warns2 (new_env, step_t) -> case checkProof (popPos (addRefs new_env refs_t step_t) (toInteger $ List.length refs_t)) elems of
            Error warns err_env err -> Error (warns++warns1++warns2) err_env err
            Ok warns3 seq_t -> Ok (warns1++warns2++warns3) seq_t

checkRefs :: [Abs.Label] -> Result [Ref]
checkRefs [] = Ok [] []
checkRefs (label: labels) = case checkRefs labels of 
    Error warns err_env err -> Error warns err_env err
    Ok warns refs_t -> case label of
        Abs.LabelRange i j -> Ok warns (RefRange i j : refs_t)
        Abs.LabelLine i    -> Ok warns (RefLine i : refs_t)

{-
    Typechecks a step in a proof, returns the environment becuase we do not change scope between each step.
    -params:
        - Environment
        - Step to check 
    -return: (Updated environment, type of the step)
-}
checkStep :: Env -> Abs.Step -> Result (Env, Arg)
checkStep env step = case step of 
    Abs.StepPrem form -> 
        case checkForm env form of
            Error warns err_env err -> Error warns err_env err
            Ok warns form_t -> Ok warns (addPrem env form_t, ArgForm form_t)
    Abs.StepFree ident -> case addFree env t of
        Error warns err_env err -> Error warns err_env err
        Ok warns new_env -> Ok warns (new_env, ArgTerm t)
        where t = Term (identToString ident) []
    Abs.StepAssume form -> 
        case checkForm env form of
            Error warns err_env err -> Error warns err_env err
            Ok warns form_t -> Ok warns (addPrem env form_t, ArgForm form_t)
    Abs.StepProof (Abs.Proof steps) -> 
        case checkProof (push env) steps of 
            Error warns err_env err -> Error warns err_env err
            Ok warns proof_t -> Ok warns (env, ArgProof proof_t)
    Abs.StepForm name args form -> 
        case checkForm env form of
            Error warns err_env err -> Error warns err_env err
            Ok warns1 form_t -> 
                case checkArgs env args of
                    Error warns err_env err -> Error (warns++warns1) err_env err
                    Ok warns2 (env1, args_t) -> 
                        case applyRule env1 (identToString name) args_t form_t of
                            Error warns err_env err -> Error (warns++warns1++warns2) err_env err
                            Ok warns3 res_t -> Ok (warns1++warns2++warns3) (env1, ArgForm res_t)
    Abs.StepNil -> Error [] env (UnknownError "Empty step.")

checkArgs :: Env -> [Abs.Arg] -> Result (Env, [Arg])
checkArgs env [] = Ok [] (env, [])
checkArgs env (arg: args) = case checkArgs env args of 
    Error warns err_env err -> Error warns err_env err
    Ok warns1 (env1, args_t) -> case checkArg env1 arg of
        Error warns err_env err -> Error (warns++warns1) err_env err
        Ok warns2 (env2, arg_t) -> Ok (warns1++warns2) (env2, arg_t : args_t)

checkArg :: Env -> Abs.Arg -> Result (Env, Arg)
checkArg env (Abs.ArgRange i j) = case getRef env (RefRange i j) of
    Error warns err_env err -> Error warns err_env err
    Ok warns arg_t -> Ok warns arg_t
checkArg env (Abs.ArgLine i) = case getRef env (RefLine i) of
    Error warns err_env err -> Error warns err_env err
    Ok warns arg_t -> Ok warns arg_t
checkArg env (Abs.ArgTerm term) = case checkTerm env term of
    Error warns err_env err -> Error warns err_env err
    Ok warns term_t -> Ok warns (env, ArgTerm term_t)
checkArg env (Abs.ArgForm term form) =case checkTerm env term of
    Error warns err_env err -> Error warns err_env err
    Ok warns1 term_t -> case checkForm env form of 
        Error warns err_env err -> Error (warns++warns1) err_env err
        Ok warns2 form_t -> Ok (warns1++warns2) (env, ArgFormWith term_t form_t)
        
{-
    Typechecks a formula
    -params:
        - Environment
        - Formula to check 
    -return: The type of the formula
-}
checkForm :: Env -> Abs.Form -> Result Formula
checkForm env f = case f of  
    Abs.FormPar form -> checkForm env form
    Abs.FormBot -> Ok [] Bot
    Abs.FormEq a b -> case checkTerm env a of
        Error warns err_env err -> Error warns err_env err
        Ok warns1 a_t -> case checkTerm env b of 
            Error warns err_env err -> Error (warns++warns1) err_env err
            Ok warns2 b_t -> Ok (warns1++warns2) (Eq a_t b_t)
    Abs.FormPred p -> case checkPred env p of
        Error warns err_env err -> Error warns err_env err
        Ok warns (_, p_t)      -> Ok warns (Pred p_t)
    Abs.FormAll ident form -> case bindVar env x of
        Error warns err_env err -> Error warns err_env err
        Ok warns1 new_env -> case checkForm new_env form of
            Error warns err_env err -> Error (warns++warns1) err_env err
            Ok warns2 term_t -> Ok (warns1++warns2) (All x term_t)
        where x = Term (identToString ident) []
    Abs.FormSome ident form -> case bindVar env x of
        Error warns err_env err -> Error warns err_env err
        Ok warns1 new_env -> case checkForm new_env form of
            Error warns err_env err -> Error (warns++warns1) err_env err
            Ok warns2 term_t -> Ok (warns1++warns2) (Some x term_t)
        where x = Term (identToString ident) []
    Abs.FormNot form        -> case checkForm env form of
        Error warns err_env err -> Error warns err_env err
        Ok warns form_t -> Ok warns (Not form_t)
    Abs.FormAnd left right -> case checkForm env left of
        Error warns err_env err -> Error warns err_env err
        Ok warns1 left_t -> case checkForm env  right of 
            Error warns err_env err -> Error (warns++warns1) err_env err
            Ok warns2 right_t -> Ok (warns1++warns2) (And left_t right_t)
    Abs.FormOr left right -> case checkForm env left of
        Error warns err_env err -> Error warns err_env err
        Ok warns1 left_t -> case checkForm env  right of 
            Error warns err_env err -> Error (warns++warns1) err_env err
            Ok warns2 right_t -> Ok (warns1++warns2) (Or left_t right_t)
    Abs.FormImpl left right  -> case checkForm env left of
        Error warns err_env err -> Error warns err_env err
        Ok warns1 left_t -> case checkForm env  right of 
            Error warns err_env err -> Error (warns++warns1) err_env err
            Ok warns2 right_t -> Ok (warns1++warns2) (Impl left_t right_t)
    Abs.FormNil -> Error [] env (TypeError "Formula is empty.")
{-
    Typechecks a predicate
    -params:
        - Environment
        - The predicate to check
    -return: The type of the predicate
-}
checkPred :: Env -> Abs.Pred -> Result (Env, Predicate)
checkPred env (Abs.Pred ident (Abs.Params terms)) = do
    -- Check all terms in the predicate
    case checkTerms env terms of
        Error warns err_env err -> Error warns err_env err
        Ok warns terms_t -> Ok warns (env, Predicate (identToString ident) terms_t)

{-
    Typechecks a term
-}
checkTerm :: Env -> Abs.Term -> Result Term
checkTerm env (Abs.Term ident (Abs.Params terms)) = case checkTerms env terms of
    Error warns err_env err -> Error warns err_env err
    Ok warns terms_t -> Ok warns (Term (identToString ident) terms_t) 
{-
    Typechecks a list of terms of an predicate or term
-}
checkTerms :: Env -> [Abs.Term] -> Result [Term]
checkTerms _ [] = Ok [] []
checkTerms env (x: xs) = case checkTerm env x of
    Error warns err_env err -> Error warns err_env err
    Ok warns1 term_t -> case checkTerms env xs of 
        Error warns err_env err -> Error (warns++warns1) err_env err
        Ok warns2 terms_t -> Ok (warns1++warns2) (term_t : terms_t)
{-
    Converts identifier to string
    -params:
        - the Abs identifier 
    -return: string
-}
identToString :: Abs.Ident -> String
identToString (Abs.Ident str) = str

{-
    Allow frontend to check sequents in background-thread
-}
handleFrontendMessage :: FrontendMessage -> BackendMessage
handleFrontendMessage (CheckStringSequent text) = StringSequentChecked (convertToFEError (checkString text))
handleFrontendMessage (CheckSequent sequent) = SequentChecked (convertToFEError (check sequent))
handleFrontendMessage (CheckStep _) = StepChecked (Left "handleFrontendMessage: CheckStep not implemented")
handleFrontendMessage (OtherFrontendMessage text) = OtherBackendMessage text

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