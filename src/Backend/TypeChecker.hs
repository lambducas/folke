module Backend.TypeChecker where

import Control.Monad
import qualified Data.Map as Map

import Logic.Abs
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
data Type = Nothing

{-
    Type for returning the result of an typecheck can either be an Type if successful or
    an error describing how the check failed 
-}
type Result = Either Error Type

{-
    Error returned from an check
-}
data Error = SyntaxError String | Unknown String deriving (Show) 

{-
    Entry point for typechecking
    sets upp env and will handle errors such
-}

isProofCorrect :: Sequent -> Bool
isProofCorrect seq = case check seq of
    Left _ ->  False
    Right _ -> True

check :: Sequent -> Result
check seq = do 
    let env = Env{prems = Map.empty}
    checkSequent env seq
{-
    Typechecks and Seq node
-}
checkSequent :: Env -> Sequent -> Result
checkSequent _ = undefined 


checkSteps :: Env -> [Step] -> Result
checkSteps env [] = undefined
checkSteps env (step:steps) = undefined

checkStep :: Env -> Step -> Result
checkStep _ = undefined 

{-
    Typechecks and Form node
-}
checkForm :: Env -> Form -> Result
checkForm env f = case f of  
    FormBot -> undefined
    FormEq term1 term2 -> undefined
    FormPred pred -> undefined
    FormAll id form -> undefined
    FormSome id form -> undefined
    FormNot form -> undefined
    FormAnd form1 form2 -> undefined
    FormOr form1 form2 -> undefined
    FormIf form1 form2 -> undefined

{-
    Typechecks and Seq node
-}
checkPred :: Env -> Pred -> Result
checkPred _ = undefined 

{-
    Typechecks and Term node
-}
checkTerm :: Env -> Term -> Result
checkTerm _ = undefined 

{-
    Typechecks and Params node
-}
checkParams :: Env -> Params -> Result
checkParams _ = undefined 

handleFrontendMessage :: FrontendMessage -> BackendMessage
handleFrontendMessage (CheckSequent sequent) =
    let result = check sequent
    in case result of
        Left err -> SequentChecked (Left (show err))
        Right _ -> SequentChecked (Left "If do not error then the proof is correct frontend do not need to know the sequent again")
handleFrontendMessage (OtherFrontendMessage text) =
    OtherBackendMessage text