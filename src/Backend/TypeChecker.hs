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
    Type containing all enviroment information for the typechecker
-}
data Env =  Env {
    prems :: [Formula],
    refs  :: Map.Map Integer Formula,
    rules :: Map.Map String ([Formula]->Formula->Result Formula)
}
newEnv :: Env
newEnv = Env{
    prems = [],
    refs  = Map.empty,
    rules = Map.fromList[
        ("Reiteration",  Rules.ruleReiteration), 
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

addPrem :: Env -> Formula -> Env
addPrem env prem = do 
    env{prems = prems env ++ [prem]}
getPrems :: Env -> [Formula]
getPrems env  = prems env

addRefs :: Env -> [Integer] -> Formula -> Env
addRefs env labels form = env{refs = Map.union (refs env) (Map.fromList [(label, form)| label <-labels])}
getRefs :: Env -> [Integer] -> Result [Formula]
getRefs env [] = Ok []
getRefs env (x: xs) = case getRefs env xs of
    Error kind msg -> Error kind msg
    Ok (forms) -> case Map.lookup x (refs env) of
        Nothing -> Error TypeError ("No ref " ++ show x ++ " exists.") 
        Just form -> Ok ([form] ++ forms)


applyRule :: Env -> String -> [Formula] -> Formula -> Result Formula
applyRule env name args res = case Map.lookup name (rules env) of
    Nothing   -> Error TypeError ("No rule named " ++ name ++ " exists.") 
    Just rule -> case rule args res of 
            Error kind msg -> Error kind msg
            Ok res_t -> if res_t == res then Ok res_t 
            else Error TypeError (show res_t ++ " did not match " ++ show res) 
{-
    Type repersenting a type in the typechecker
-}
{-
    Type for returning the result of an typecheck can either be an Type if successful or
    an error describing how the check failed 
-}
{-
    Entry point for typechecking
    sets upp env and will handle errors such
-}

checkString :: String -> Result ()
checkString proof = case pSequent (myLexer proof) of
    Left err -> Error SyntaxError err
    Right seq_t -> check seq_t

isProofCorrect :: Abs.Sequent -> Bool
isProofCorrect seq_t = case check seq_t of
    Error _ _->  False
    Ok _ -> True

check :: Abs.Sequent -> Result ()
check seq_t = do 
    let env = newEnv
    case checkSequent env seq_t of
        Error kind msg -> Error kind msg
        Ok _ -> Ok ()
{-
    Typechecks and Seq node
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


checkProof :: Env -> [Abs.ProofElem] -> Result Proof
checkProof env [] = Ok (Proof (getPrems env) Nil)
checkProof env [Abs.ProofElem _ step] = case checkStep env step of
    Error kind msg -> Error kind msg
    Ok (new_env, step_t) -> Ok (Proof (getPrems new_env) step_t)
checkProof env ((Abs.ProofElem labels step):elems) = case checkStep env step of
    Error kind msg -> Error kind msg
    Ok (new_env, step_t) -> case checkProof (addRefs new_env (List.reverse[i| (Abs.Label i) <- labels]) step_t) elems of
        Error kind msg -> Error kind msg
        Ok seq_t -> Ok seq_t


checkStep :: Env -> Abs.Step -> Result (Env, Formula)
checkStep env step = case step of 
    Abs.StepPrem     form         -> case checkForm env form of
        Error kind msg -> Error kind msg
        Ok form_t      -> Ok (addPrem env form_t, form_t)
    Abs.StepDecConst id           -> Error UnknownError "Unimplemented checkStep DecConst"
    Abs.StepDecVar   id           -> Error UnknownError "Unimplemented checkStep DecVar"
    Abs.StepDecFun   id ids       -> Error UnknownError "Unimplemented checkStep DecFun"
    Abs.StepAssume   form         -> Error UnknownError "Unimplemented checkStep Assume"
    Abs.StepProof    steps        -> Error UnknownError "Unimplemented checkStep Proof"
    Abs.StepForm     name args form -> case checkForm env form of
        Error kind msg -> Error kind msg
        Ok form_t -> case getRefs env ([arg| (Abs.ArgLit arg) <- args]) of
             Error kind msg -> Error kind msg
             Ok refs_t -> case applyRule env (identToString name) refs_t form_t of
                Error kind msg -> Error kind msg
                Ok res_t -> Ok(env, res_t)


checkForms :: Env -> [Abs.Form] -> Result [Formula]
checkForms _ []           = Ok []
checkForms env (form:forms) = case checkForm env form of 
    Error kind msg -> Error kind msg
    Ok form_t      -> case checkForms env forms of 
        Error kind msg -> Error kind msg
        Ok forms_t     -> Ok ([form_t] ++ forms_t)

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
    Typechecks and Seq node
-}
checkPred :: Env -> Abs.Pred -> Result Predicate
checkPred env (Abs.Pred id (Abs.Params params)) = Ok (Predicate (identToString id)) 

{-
    Typechecks and Term node
-}
checkTerm :: Env -> Abs.Term -> Result ()
checkTerm env term = Error UnknownError "Unimplemented checkTerm"  

{-
    Typechecks and Params node
-}
checkParams :: Env -> Abs.Params -> Result ()
checkParams env params =  Error UnknownError "Unimplemented checkParams"   

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