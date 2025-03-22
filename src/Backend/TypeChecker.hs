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
import Backend.Types 
import qualified Backend.Rules as Rules
import qualified Data.List as List
{-
    Type containing all environment information for an instace of the typechecker.
    -prems: Contains a list of all premises/assumptions in the current scope in the order they were introduced
    -refs:  References to all labeld steps in the proof. 
    -rules: All rules 
-}
data Env =  Env {
    prems :: [Formula],
    refs  :: Map.Map String Arg,
    rules :: Map.Map String ([Arg]->Formula->Result Formula)
}
{-
    Creates a new environment
    -params:
    -return: New environment
-}
newEnv :: Env
newEnv = Env{
    prems = [],
    refs  = Map.empty,
    rules = Map.fromList[
        ("copy",  Rules.ruleCopy), 
        ("AndI", Rules.ruleAndIntro),
        ("AndEL", Rules.ruleAndElimLeft),
        ("AndER", Rules.ruleAndElimRight),
        ("OrIL", Rules.ruleOrIntroLeft),
        ("OrIR", Rules.ruleOrIntroRight),
        ("OrE", Rules.ruleOrEilm),
        ("IfI", Rules.ruleIfIntro),
        ("IfE", Rules.ruleIfEilm),
        ("NotI", Rules.ruleNotIntro),
        ("NotE", Rules.ruleNotEilm),
        ("BotE", Rules.ruleBottomElim),
        ("NotNotI", Rules.ruleNotNotIntro),
        ("NotNotE", Rules.ruleNotNotElim),
        ("MT", Rules.ruleMT),
        ("PBC", Rules.rulePBC),
        ("LEM", Rules.ruleLEM)
        ]
    }
{-
    Pushes an new context to the environment, used when entering a box
    -params:
        -Old environment
    -return: Updated environment
    -}
push :: Env -> Env
push env = env {prems=[]}

{-
    Adds an premise/assumption to the environment
    -params:
        - Old environment
        - Premise/assumption to be added
    -return: Updated environment
-}
addPrem :: Env -> Formula -> Env
addPrem env prem = do 
    env{prems = prems env ++ [prem]}
{-
    Returns all premises/assumptions in the current scope
    -params:
        - Environment
    -return: List of all premises/assumptions 
-}
getPrems :: Env -> [Formula]
getPrems env  = prems env

{-
    Adds references
    -params:
        - Old environment
        - List of labels
        - The value of the labels(all labels get the same value)
    -return: Updated environment
-}
addRefs :: Env -> [String] -> Arg -> Env
addRefs env labels form = env{refs = Map.union (refs env) (Map.fromList [(label, form)| label <-labels])}

{-
    Returns the values corresponding to the refrenses in an list
    -params:
        - Environment
        - List of labels
    -return: List of values associated with the labels
-}
getRefs :: Env -> [String] -> Result [Arg]
getRefs _ [] = Ok []
getRefs env (x: xs) = case getRefs env xs of
    Error kind msg -> Error kind msg
    Ok forms -> case Map.lookup x (refs env) of
        Nothing -> Error TypeError ("No ref " ++ x ++ " exists.") 
        Just form -> Ok ([form] ++ forms)

{-
    Applies rule to a given list of arguments will return error if expected result dont match result.
    -params:
        - Environment
        - Name of the rule
        - List of arguments to apply to the rule
        - Expected result of the application
    -return: The result of the application of the rule
-}
applyRule :: Env -> String -> [Arg] -> Formula -> Result Formula
applyRule env name args res = case Map.lookup name (rules env) of
    Nothing   -> Error TypeError ("No rule named " ++ name ++ " exists.") 
    Just rule -> case rule args res of 
            Error kind msg -> Error kind  ("While applying rule " ++ name ++ ": " ++ msg)
            Ok res_t -> if res_t == res then Ok res_t 
            else Error TypeError ("Wrong conclusion when using rule " ++ name ++ " expected " ++ show res_t ++ " not " ++ show res) 

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
    Error kind msg -> Error kind (show (List.reverse["@" ++ i | (Abs.Label (Abs.LabelToken i)) <- labels]) ++ msg)
    Ok (new_env, step_t) -> case checkProof (addRefs new_env (List.reverse[i| (Abs.Label (Abs.LabelToken i)) <- labels]) step_t) elems of
        Error kind msg -> Error kind msg
        Ok seq_t -> Ok seq_t

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
        Ok form_t -> case getRefs env ([arg | (Abs.ArgLit (Abs.LabelToken arg)) <- args]) of
             Error kind msg -> Error kind msg
             Ok refs_t -> case applyRule env (identToString name) refs_t form_t of
                Error kind msg -> Error kind msg
                Ok res_t -> Ok(env, ArgForm res_t)

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
    Abs.FormEq term1 term2  -> Error UnknownError "Unimplemented checkForm eq"
    Abs.FormPred pred       -> case checkPred env pred of
        Error kind msg  -> Error kind msg
        Ok pred_t       -> Ok (Pred pred_t)
    Abs.FormAll id form     -> Error UnknownError "Unimplemented checkForm all"
    Abs.FormSome id form    -> Error UnknownError "Unimplemented checkForm some"
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
checkTerm :: Env -> Abs.Term -> Result ()
checkTerm env term = Error UnknownError "Unimplemented checkTerm"  

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