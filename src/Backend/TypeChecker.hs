module Backend.TypeChecker where

import Control.Monad
import qualified Data.Map as Map

import qualified Logic.Abs as Abs
import Logic.Par (pSequent, myLexer)
import Shared.Messages
{-
    Type containing all enviroment information for the typechecker
-}
data Env =  Env {
    prems :: Map.Map String Type
}
addPrem :: Env -> String -> Type -> Env
addPrem env id t = do 
    let p = prems env
    env{prems = Map.insert id t p}

{-
    Type repersenting a type in the typechecker
-}
{-
    Type for returning the result of an typecheck can either be an Type if successful or
    an error describing how the check failed 
-}
data Result = Ok Type | Error ErrorKind String

data ErrorKind = SyntaxError | UnknownError
data Type = Form | Pred
{-
    Entry point for typechecking
    sets upp env and will handle errors such
-}

isProofCorrect :: Abs.Sequent -> Bool
isProofCorrect seq = case check seq of
    Error _ _->  False
    Ok _ -> True

check :: Abs.Sequent -> Result
check seq = do 
    let env = Env{prems = Map.empty}
    checkSequent env seq
{-
    Typechecks and Seq node
-}
checkSequent :: Env -> Abs.Sequent -> Result
checkSequent env seq= Error UnknownError "Unimplemented"


checkSteps :: Env -> [Abs.Step] -> Result
checkSteps env [] = undefined
checkSteps env (step:steps) = undefined

checkStep :: Env -> Abs.Step -> Result
checkStep _ = undefined 

{-
    Typechecks and Form node
-}
checkForms :: Env -> [Abs.Form] -> Result
checkForms env forms= Error UnknownError "Unimplemented"

checkForm :: Env -> Abs.Form -> Result
checkForm env f = case f of  
    Abs.FormBot -> undefined
    Abs.FormEq term1 term2 -> undefined
    Abs.FormPred pred -> undefined
    Abs.FormAll id form -> undefined
    Abs.FormSome id form -> undefined
    Abs.FormNot form -> undefined
    Abs.FormAnd form1 form2 -> undefined
    Abs.FormOr form1 form2 -> undefined
    Abs.FormIf form1 form2 -> undefined

{-
    Typechecks and Seq node
-}
checkPred :: Env -> Abs.Pred -> Result
checkPred _ = undefined 

{-
    Typechecks and Term node
-}
checkTerm :: Env -> Abs.Term -> Result
checkTerm _ = undefined 

{-
    Typechecks and Params node
-}
checkParams :: Env -> Abs.Params -> Result
checkParams _ = undefined 

handleFrontendMessage :: FrontendMessage -> BackendMessage
handleFrontendMessage (CheckSequent sequent) =
    let result = check sequent
    in case result of
        Error _ msg -> SequentChecked (Left msg)
        Ok _ -> SequentChecked (Left "If do not error then the proof is correct frontend do not need to know the sequent again")
handleFrontendMessage (OtherFrontendMessage text) =
    OtherBackendMessage text