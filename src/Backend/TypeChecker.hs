module Backend.TypeChecker (
    isProofCorrect,
    check,
    checkString,
    handleFrontendMessage,
) where

import qualified Data.Map as Map

import qualified Logic.Abs as Abs
import Logic.Par (pSequent, myLexer)
import Shared.Messages
import Backend.Environment
import Backend.Types 
import qualified Data.List as List
{-
    Runs the parser and then the typechecker on a given string
    -params:
        - Code to check
    -return: Ok/Error
-}
checkString :: String -> Result ()
checkString proof = case pSequent (myLexer proof) of
    Left err -> Error [] SyntaxError err
    Right seq_t -> check seq_t
{-
    Typechecks if a given proof is correct and if it matches the sequent. Discards any error information
    -params:
        - Sequent to check
    -return: Yes/No
-}
isProofCorrect :: Abs.Sequent -> Bool
isProofCorrect seq_t = case check seq_t of
    Error _ _ _->  False
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
        Error warns kind msg -> Error warns kind msg
        Ok warns _ -> Ok warns ()
{-
    Typechecks a given proof and if it matches the sequent and returns what it proves
    -params:
        - environment
        - Sequent to check 
    -return: The type of the proof
-}
checkSequent :: Env -> Abs.Sequent -> Result Proof
checkSequent env (Abs.Seq prems conc (Abs.Proof proof)) = case checkForms env prems of 
    Error warns kind msg -> Error warns kind msg
    Ok warns1 prems_t -> case checkForm env conc of 
         Error warns kind msg -> Error (warns++warns1) kind msg
         Ok warns2 conc_t -> case checkProof env proof of
             Error warns kind msg -> Error (warns++warns1++warns2) kind msg
             Ok warns3 proof_t -> do 
                let seq_t = Proof prems_t conc_t
                if proof_t == seq_t then Ok (warns1++warns2++warns3) seq_t
                else Error (warns1++warns2++warns3) TypeError ("The proof " ++ show proof_t ++ " did not match the expected " ++ show seq_t ++ ".")


{-
    Typechecks a given proof
    -params:
        - environment
        - List of steps in the proof
    -return: The type of the proof
-}
checkProof :: Env -> [Abs.ProofElem] -> Result Proof
checkProof env [] = Ok [] (Proof (getPrems env) Nil)
checkProof env [Abs.ProofElem _ step] = case checkStep env step of
    Error warns kind msg -> Error warns kind msg
    Ok warns (_, ArgProof _) -> Error warns TypeError "Last step in proof was another proof."
    Ok warns (new_env, ArgForm step_t) -> Ok warns (Proof (getPrems new_env) step_t)
    Ok warns (_, ArgTerm _) -> Error warns TypeError "Check step could not return an term."
checkProof env ((Abs.ProofElem labels step):elems) = case checkStep env step of
    Error warns kind msg -> Error warns kind (show (List.reverse["@" ++ show i| i <- labels]) ++ msg)
    Ok warns (_, ArgTerm _) -> Error warns TypeError "Check step could not return an term."
    Ok warns1 (new_env, step_t) -> case checkRefs labels of
        Error warns kind msg -> Error (warns++warns1) kind msg
        Ok warns2 refs -> case checkProof (addRefs new_env refs step_t) elems of
            Error warns kind msg -> Error (warns++warns1++warns2) kind msg
            Ok warns3 seq_t -> Ok (warns1++warns2++warns3) seq_t

checkRefs :: [Abs.Label] -> Result [Ref]
checkRefs [] = Ok [] []
checkRefs (label: labels) = case checkRefs labels of 
    Error warns kind msg -> Error warns kind msg
    Ok warns refs -> case label of
        Abs.LabelRange i j -> Ok warns ([RefRange i j] ++ refs)
        Abs.LabelLine i    -> Ok warns ([RefLine i] ++ refs)

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
            Error warns kind msg -> Error warns kind msg
            Ok warns form_t -> Ok warns (addPrem env form_t, ArgForm form_t)
    Abs.StepDecConst id -> 
        let updatedEnv = addConst env (identToString id)
            constTerm = Term (identToString id) []
        in Ok [] (updatedEnv, ArgTerm constTerm)
    Abs.StepDecVar id -> 
        let updatedEnv = addVar env (identToString id)
            varTerm = Term (identToString id) []
        in Ok [] (updatedEnv, ArgTerm varTerm)
    Abs.StepDecFun id ids -> 
        let updatedEnv = addFun env (identToString id) (map identToString ids)
            funTerm = Term (identToString id) (map (\arg -> Term (identToString arg) []) ids)
        in Ok [] (updatedEnv, ArgTerm funTerm)
    Abs.StepAssume form -> 
        case checkForm env form of
            Error warns kind msg -> Error warns kind msg
            Ok warns form_t -> Ok warns (addPrem env form_t, ArgForm form_t)
    Abs.StepProof (Abs.Proof steps) -> 
        case checkProof (push env) steps of 
            Error warns kind msg -> Error warns kind msg
            Ok warns proof_t -> Ok warns (env, ArgProof proof_t)
    Abs.StepForm name args form -> 
        case checkForm env form of
            Error warns kind msg -> Error warns kind msg
            Ok warns1 form_t -> 
                case checkArgs env args of
                    Error warns kind msg -> Error (warns++warns1) kind msg
                    Ok warns2 args_t -> 
                        case applyRule env (identToString name) args_t form_t of
                            Error warns kind msg -> Error (warns++warns1++warns2) kind msg
                            Ok warns3 res_t -> Ok (warns1++warns2++warns3) (env, ArgForm res_t)
    Abs.StepNil -> Error [] UnknownError "Empty step."

checkArgs :: Env -> [Abs.Arg] -> Result [Arg]
checkArgs env [] = Ok [] []
checkArgs env (arg: args) = case checkArg env arg of
    Error warns kind msg -> Error warns kind msg
    Ok warns1 arg_t -> case checkArgs env args of 
        Error warns kind msg -> Error (warns++warns1) kind msg
        Ok warns2 args_t -> Ok (warns1++warns2) (arg_t : args_t)
checkArg :: Env -> Abs.Arg -> Result Arg
checkArg env (Abs.ArgRange i j) = case getRef env (RefRange i j) of
    Error warns kind msg -> Error warns kind msg
    Ok warns arg_t -> Ok warns arg_t
checkArg env (Abs.ArgLine i) = case getRef env (RefLine i) of
    Error warns kind msg -> Error warns kind msg
    Ok warns arg_t -> Ok warns arg_t
checkArg env (Abs.ArgTerm term) = case checkTerm env term of
    Error warns kind msg -> Error warns kind msg
    Ok warns term_t -> Ok warns (ArgTerm term_t)
{-
    Typechecks a list of formulas, helper function when we need to check several formuals at the same time.
    -params:
        - Environment
        - Formulas to check 
    -return: List with the types of the formulas
-}
checkForms :: Env -> [Abs.Form] -> Result [Formula]
checkForms _ []           = Ok [] []
checkForms env (form:forms) = case checkForm env form of
    Error warns kind msg -> Error warns kind msg
    Ok warns1 form_t      -> case checkForms env forms of
        Error warns kind msg -> Error (warns++warns1) kind msg
        Ok warns2 forms_t     -> Ok (warns1++warns2) (form_t : forms_t)
        
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
        Error warns kind msg -> Error warns kind msg
        Ok warns1 a_t -> case checkTerm env b of 
            Error warns kind msg -> Error (warns++warns1) kind msg
            Ok warns2 b_t -> Ok (warns1++warns2) (Eq a_t b_t)
    Abs.FormPred pred -> case checkPred env pred of
        Error warns kind msg -> Error warns kind msg
        Ok warns pred_t      -> Ok warns (Pred pred_t)
    Abs.FormAll id form -> case checkForm env form of
        Error warns kind msg -> Error warns kind msg
        Ok warns term_t -> Ok warns (All (Term (identToString id) []) term_t)
    Abs.FormSome id form -> case checkForm env form of
        Error warns kind msg -> Error warns kind msg
        Ok warns term_t -> Ok warns (All (Term (identToString id) []) term_t)
    Abs.FormNot form        -> case checkForm env form of
        Error warns kind msg -> Error warns kind msg
        Ok warns form_t -> Ok warns (Not form_t)
    Abs.FormAnd left right -> case checkForm env left of
        Error warns kind msg -> Error warns kind msg
        Ok warns1 left_t -> case checkForm env  right of 
            Error warns kind msg -> Error (warns++warns1) kind msg
            Ok warns2 right_t -> Ok (warns1++warns2) (And left_t right_t)
    Abs.FormOr left right -> case checkForm env left of
        Error warns kind msg -> Error warns kind msg
        Ok warns1 left_t -> case checkForm env  right of 
            Error warns kind msg -> Error (warns++warns1) kind msg
            Ok warns2 right_t -> Ok (warns1++warns2) (Or left_t right_t)
    Abs.FormImpl left right  -> case checkForm env left of
        Error warns kind msg -> Error warns kind msg
        Ok warns1 left_t -> case checkForm env  right of 
            Error warns kind msg -> Error (warns++warns1) kind msg
            Ok warns2 right_t -> Ok (warns1++warns2) (Impl left_t right_t)
{-
    Typechecks a predicate
    -params:
        - Environment
        - The predicate to check
    -return: The type of the predicate
-}
checkPred :: Env -> Abs.Pred -> Result Predicate
checkPred env (Abs.Pred id (Abs.Params terms)) = case checkTerms env terms of
    Error warns kind msg -> Error warns kind msg
    Ok warns terms_t -> Ok warns (Predicate (identToString id) terms_t) 

{-
    Typechecks a term
-}
checkTerm :: Env -> Abs.Term -> Result Term
checkTerm env (Abs.Term id (Abs.Params terms)) = case checkTerms env terms of
    Error warns kind msg -> Error warns kind msg
    Ok warns terms_t -> Ok warns (Term (identToString id) terms_t) 
{-
    Typechecks a list of terms of an predicate or term
-}
checkTerms :: Env -> [Abs.Term] -> Result [Term]
checkTerms _ [] = Ok [] []
checkTerms env (x: xs) = case checkTerm env x of
    Error warns kind msg -> Error warns kind msg
    Ok warns1 term_t -> case checkTerms env xs of 
        Error warns kind msg -> Error (warns++warns1) kind msg
        Ok warns2 terms_t -> Ok (warns1++warns2) (term_t : terms_t)
{-
    Converts identifier to string
    -params:
        - the Abs identifier 
    -return: string
-}
identToString :: Abs.Ident -> String
identToString (Abs.Ident str) = str

handleFrontendMessage :: FrontendMessage -> BackendMessage
handleFrontendMessage (CheckStringSequent text) =
    let result = checkString text
    in case result of
        Error _ kind msg -> StringSequentChecked (Left (show kind ++ ": " ++ msg))
        Ok _ _ -> StringSequentChecked (Right ())
handleFrontendMessage (CheckSequent sequent) =
    let result = check sequent
    in case result of
        Error _ kind msg -> StringSequentChecked (Left (show kind ++ ": " ++ msg))
        Ok _ _ -> SequentChecked (Right ())
handleFrontendMessage (CheckStep _) =
    StepChecked (Left "handleFrontendMessage: CheckStep not implemented")
handleFrontendMessage (OtherFrontendMessage text) =
    OtherBackendMessage text