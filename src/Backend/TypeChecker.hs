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
    Left err -> Error SyntaxError err
    Right seq_t -> check seq_t
{-
    Typechecks if a given proof is correct and if it matches the sequent. Discards any error information
    -params:
        - Sequent to check
    -return: Yes/No
-}
isProofCorrect :: Abs.Sequent -> Bool
isProofCorrect seq_t = case check seq_t of
    Error _ _->  False
    Ok _ -> True

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
        Error kind msg -> Error kind msg
        Ok _ -> Ok ()
{-
    Typechecks a given proof and if it matches the sequent and returns what it proves
    -params:
        - environment
        - Sequent to check 
    -return: The type of the proof
-}
checkSequent :: Env -> Abs.Sequent -> Result Proof
checkSequent env (Abs.Seq prems conc (Abs.Proof proof)) = case checkForms env prems of 
    Error kind msg -> Error kind msg
    Ok prems_t -> case checkForm env conc of 
         Error kind msg -> Error kind msg
         Ok conc_t -> case checkProof env proof of
             Error kind msg -> Error kind msg
             Ok proof_t -> do 
                let seq_t = Proof prems_t conc_t
                if proof_t == seq_t then Ok seq_t
                else Error TypeError ("The proof " ++ show proof_t ++ " did not match the expected " ++ show seq_t ++ ".")


{-
    Typechecks a given proof
    -params:
        - environment
        - List of steps in the proof
    -return: The type of the proof
-}
checkProof :: Env -> [Abs.ProofElem] -> Result Proof
checkProof env [] = Ok (Proof (getPrems env) Nil)
checkProof env [Abs.ProofElem _ step] = case checkStep env step of
    Error kind msg -> Error kind msg
    Ok (_, ArgProof _) -> Error TypeError "Last step in proof was another proof."
    Ok (new_env, ArgForm step_t) -> Ok (Proof (getPrems new_env) step_t)
checkProof env ((Abs.ProofElem labels step):elems) = case checkStep env step of
    Error kind msg -> Error kind (show (List.reverse["@" ++ show i| i <- labels]) ++ msg)
    Ok (new_env, step_t) -> case checkRefs labels of
        Error kind msg -> Error kind msg
        Ok refs -> case checkProof (addRefs new_env refs step_t) elems of
            Error kind msg -> Error kind msg
            Ok seq_t -> Ok seq_t

checkRefs :: [Abs.Label] -> Result [Ref]
checkRefs [] = Ok[]
checkRefs (label: labels) = case checkRefs labels of 
    Error kind msg -> Error kind msg
    Ok refs -> case label of
        Abs.LabelRange i j -> Ok ([RefRange i j] ++ refs)
        Abs.LabelLine i    -> Ok ([RefLine i] ++ refs)

{-
    Typechecks a step in a proof, returns the environment becuase we do not change scope between each step.
    -params:
        - Environment
        - Step to check 
    -return: (Updated environment, type of the step)
-}
checkStep :: Env -> Abs.Step -> Result (Env, Arg)
checkStep env step = case step of 
    Abs.StepPrem     form         -> case checkForm env form of
        Error kind msg -> Error kind ("Inside assumption: "++ msg)
        Ok form_t      -> Ok (addPrem env form_t, ArgForm form_t)
    Abs.StepDecConst id           -> Error UnknownError "Unimplemented checkStep DecConst"
    Abs.StepDecVar   id           -> Error UnknownError "Unimplemented checkStep DecVar"
    Abs.StepDecFun   id ids       -> Error UnknownError "Unimplemented checkStep DecFun"
    Abs.StepAssume   form         -> case checkForm env form of
        Error kind msg -> Error kind ("Inside assumption: "++ msg)
        Ok form_t      -> Ok (addPrem env form_t, ArgForm form_t)
    Abs.StepProof   (Abs.Proof steps) -> case checkProof (push env) steps of 
        Error kind msg -> Error kind ("Inside subproof: "++msg)
        Ok proof_t -> Ok(env, ArgProof proof_t)
    Abs.StepForm     name args form -> case checkForm env form of
        Error kind msg -> Error kind ("While checking given result: " ++ msg)
        Ok form_t -> case checkArgs env args of
            Error kind msg -> Error kind msg
            Ok refs ->case getRefs env refs of
                Error kind msg -> Error kind msg
                Ok refs_t -> case applyRule env (identToString name) refs_t form_t of
                    Error kind msg -> Error kind msg
                    Ok res_t -> Ok(env, ArgForm res_t)

checkArgs :: Env -> [Abs.Arg] -> Result [Ref]
checkArgs env [] = Ok []
checkArgs env (arg: args) = case checkArgs env args of 
    Error kind msg -> Error kind msg
    Ok refs -> case arg of 
        Abs.ArgRange i j -> Ok ([RefRange i j] ++ refs)
        Abs.ArgLine  i   -> Ok ([RefLine i] ++ refs)
{-
    Typechecks a list of formulas, helper function when we need to check several formuals at the same time.
    -params:
        - Environment
        - Formulas to check 
    -return: List with the types of the formulas
-}
checkForms :: Env -> [Abs.Form] -> Result [Formula]
checkForms _ []           = Ok []
checkForms env (form:forms) = case checkForm env form of 
    Error kind msg -> Error kind msg
    Ok form_t      -> case checkForms env forms of 
        Error kind msg -> Error kind msg
        Ok forms_t     -> Ok ([form_t] ++ forms_t)
        
{-
    Typechecks a formula
    -params:
        - Environment
        - Formula to check 
    -return: The type of the formula
-}
checkForm :: Env -> Abs.Form -> Result Formula
checkForm env f = case f of  
    Abs.FormBot             -> Ok Bot
    Abs.FormEq a b -> case checkTerm env a of
        Error kind msg -> Error kind msg
        Ok a_t -> case checkTerm env b of 
            Error kind msg -> Error kind msg
            Ok b_t -> Ok (Eq a_t b_t)
    Abs.FormPred pred -> case checkPred env pred of
        Error kind msg -> Error kind msg
        Ok pred_t      -> Ok (Pred pred_t)
    Abs.FormAll id form -> Error UnknownError "Unimplemented checkForm all"
    Abs.FormSome id form -> Error UnknownError "Unimplemented checkForm some"
    Abs.FormNot form        -> case checkForm env form of
        Error kind msg -> Error kind msg
        Ok form_t -> Ok (Not form_t)
    Abs.FormAnd left right -> case checkForm env left of
        Error kind msg -> Error kind msg
        Ok left_t -> case checkForm env  right of 
            Error kind msg -> Error kind msg
            Ok right_t -> Ok (And left_t right_t)
    Abs.FormOr left right -> case checkForm env left of
        Error kind msg -> Error kind msg
        Ok left_t -> case checkForm env  right of 
            Error kind msg -> Error kind msg
            Ok right_t -> Ok (Or left_t right_t)
    Abs.FormIf left right  -> case checkForm env left of
         Error kind msg -> Error kind msg
         Ok left_t -> case checkForm env  right of 
            Error kind msg -> Error kind msg
            Ok right_t -> Ok (If left_t right_t)
{-
    Typechecks a predicate
    -params:
        - Environment
        - The predicate to check
    -return: The type of the predicate
-}
checkPred :: Env -> Abs.Pred -> Result Predicate
checkPred env (Abs.Pred id (Abs.Params params)) = Ok (Predicate (identToString id)) 

{-
    Typechecks a term
-}
checkTerm :: Env -> Abs.Term -> Result Term
checkTerm env (Abs.Term id (Abs.Params params)) = Ok (Term (identToString id))

{-
    Typechecks a list of parameters of an predicate or term
-}
checkParams :: Env -> Abs.Params -> Result ()
checkParams env params =  Error UnknownError "Unimplemented checkParams"

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
        Error kind msg -> StringSequentChecked (Left (show kind ++ ": " ++ msg))
        Ok _ -> StringSequentChecked (Right ())
handleFrontendMessage (CheckSequent sequent) =
    let result = check sequent
    in case result of
        Error kind msg -> StringSequentChecked (Left (show kind ++ ": " ++ msg))
        Ok _ -> SequentChecked (Right ())
handleFrontendMessage (CheckStep _) =
    StepChecked (Left "handleFrontendMessage: CheckStep not implemented")
handleFrontendMessage (OtherFrontendMessage text) =
    OtherBackendMessage text