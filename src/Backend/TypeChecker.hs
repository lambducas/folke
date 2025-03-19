module Backend.TypeChecker (
    isProofCorrect,
    check,
    checkString,
    handleFrontendMessage,
    Result(Ok, Error),
    ErrorKind(TypeError,SyntaxError, UnknownError)
) where

import Control.Monad
import qualified Data.Map as Map

import qualified Logic.Abs as Abs
import Logic.Par (pSequent, myLexer)
import Shared.Messages
{-
    Type containing all enviroment information for the typechecker
-}
data Env =  Env {
    prems :: [Formula],
    refs  :: Map.Map Int Formula,
    rules :: Map.Map String ([Formula]->Formula)
}
newEnv :: Env
newEnv = Env{
    prems = [],
    refs  = Map.empty,
    rules = Map.empty
    }

addPrem :: Env -> Formula -> Env
addPrem env prem = do 
    env{prems = [prem] ++ prems env}
getPrems :: Env -> [Formula]
getPrems env  = prems env

applyRule :: Env -> String -> [Formula] -> Result Formula
applyRule env id args = case Map.lookup id (rules env) of
    Nothing   -> Error TypeError ("No rule named " ++ id ++ " exists.") 
    Just rule -> Ok (rule args)
{-
    Type repersenting a type in the typechecker
-}
{-
    Type for returning the result of an typecheck can either be an Type if successful or
    an error describing how the check failed 
-}
data Result a = Ok a | Error ErrorKind String

data Sequent = Sequent [Formula] Formula

instance Eq Sequent where 
    Sequent [] conc_a == Sequent [] conc_b = conc_a == conc_b
    Sequent [a] conc_a == Sequent [b] conc_b = a == b && Sequent [] conc_a == Sequent [] conc_b
    Sequent [] _ == Sequent _ _ = False
    Sequent _ _ == Sequent [] _ = False
    Sequent (a:as) conc_a == Sequent (b:bs) conc_b = a == b && Sequent as conc_a == Sequent bs conc_b

data Formula = 
            Pred Predicate |
            And Formula Formula |
            Nil
instance Eq Formula where 
    And a1 a2 == And b1 b2 = a1 == b1 && a2 == b2
    Pred a == Pred b = a==b
    _ == _ = False


data Predicate = Predicate String
instance Eq Predicate where 
    Predicate a == Predicate b = a == b

data ErrorKind = TypeError | SyntaxError | UnknownError deriving Show 
{-
    Entry point for typechecking
    sets upp env and will handle errors such
-}

checkString :: String -> Result ()
checkString proof = case pSequent (myLexer proof) of
    Left err -> Error SyntaxError err
    Right seq -> check seq

isProofCorrect :: Abs.Sequent -> Bool
isProofCorrect seq = case check seq of
    Error _ _->  False
    Ok _ -> True

check :: Abs.Sequent -> Result ()
check seq = do 
    let env = newEnv
    case checkSequent env seq of
        Error kind msg -> Error kind msg
        Ok _ -> Ok ()
{-
    Typechecks and Seq node
-}
checkSequent :: Env -> Abs.Sequent -> Result Sequent
checkSequent env (Abs.Seq prems conc steps) = case checkForms env prems of 
    Error kind msg -> Error kind msg
    Ok prems_t -> case checkForm env conc of 
         Error kind msg -> Error kind msg
         Ok conc_t -> case checkSteps env steps of
             Error kind msg -> Error kind msg
             Ok proof_t -> do 
                let seq_t = Sequent prems_t conc_t
                if proof_t == seq_t then Ok seq_t
                else Error TypeError "The proof steps did not prove the sequent"


checkSteps :: Env -> [Abs.Step] -> Result Sequent
checkSteps env [] = Ok (Sequent (getPrems env) Nil) 
checkSteps env [step] = case step of 
    Abs.StepPrem labels form         -> case checkForm env form of
        Error kind msg -> Error kind msg
        Ok form_t      -> Ok (Sequent (getPrems (addPrem env form_t)) form_t) 
    Abs.StepDecConst labels id       -> Error UnknownError "Unimplemented checkSteps DecConst"
    Abs.StepDecVar labels id         -> Error UnknownError "Unimplemented checkSteps DecVar"
    Abs.StepDecFun labels id ids     -> Error UnknownError "Unimplemented checkSteps DecFun"
    Abs.StepAssume labels form       -> Error UnknownError "Unimplemented checkSteps Assume"
    Abs.StepProof labels steps       -> Error UnknownError "Unimplemented checkSteps Proof"
    Abs.StepForm labels id args form -> Error UnknownError "Unimplemented checkSteps From"
checkSteps env (step:steps) = case step of 
    Abs.StepPrem labels form         -> case checkForm env form of
        Error kind msg -> Error kind msg
        Ok form_t      -> checkSteps (addPrem env form_t) steps
    Abs.StepDecConst labels id       -> Error UnknownError "Unimplemented checkSteps DecConst"
    Abs.StepDecVar labels id         -> Error UnknownError "Unimplemented checkSteps DecVar"
    Abs.StepDecFun labels id ids     -> Error UnknownError "Unimplemented checkSteps DecFun"
    Abs.StepAssume labels form       -> Error UnknownError "Unimplemented checkSteps Assume"
    Abs.StepProof labels steps       -> Error UnknownError "Unimplemented checkSteps Proof"
    Abs.StepForm labels id args form -> Error UnknownError "Unimplemented checkSteps From"

{-
    Typechecks and Form node
-}
checkForms :: Env -> [Abs.Form] -> Result [Formula]
checkForms _ []           = Ok []
checkForms env (form:forms) = case checkForm env form of 
    Error kind msg -> Error kind msg
    Ok form_t      -> case checkForms env forms of 
        Error kind msg -> Error kind msg
        Ok forms_t     -> Ok ([form_t] ++ forms_t)

checkForm :: Env -> Abs.Form -> Result Formula
checkForm env f = case f of  
    Abs.FormBot             -> Error UnknownError "Unimplemented checkForm bot"
    Abs.FormEq term1 term2  -> Error UnknownError "Unimplemented checkForm eq"
    Abs.FormPred pred       -> case checkPred env pred of
        Error kind msg  -> Error kind msg
        Ok pred_t       -> Ok (Pred pred_t)
    Abs.FormAll id form     -> Error UnknownError "Unimplemented checkForm all"
    Abs.FormSome id form    -> Error UnknownError "Unimplemented checkForm some"
    Abs.FormNot form        -> Error UnknownError "Unimplemented checkForm not"
    Abs.FormAnd form1 form2 -> Error UnknownError "Unimplemented checkForm and"
    Abs.FormOr form1 form2  -> Error UnknownError "Unimplemented checkForm or"
    Abs.FormIf form1 form2  -> Error UnknownError "Unimplemented checkForm if"

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