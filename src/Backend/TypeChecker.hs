{-# LANGUAGE GADTs #-}
module Backend.TypeChecker (
    checkTerm,
    checkForm,
    checkStep,
    checkSequent,
    checkSteps,
    ruleIdToString,
    termIdToString,
    isProofCorrect
) where

import Parser.Logic.Abs
import qualified Data.Map as Map
import Backend.Rules

type Environment = Map.Map String Term
type LineMap = Map.Map Integer Form
type Result a = Either String (a, Environment)

checkTerm :: Environment -> Term -> Result Term
checkTerm env term = case term of
    Term termId params -> case Map.lookup (termIdToString termId) env of
        Just t -> Right (t, env)
        Nothing -> Right (term, env)

checkForm :: Environment -> Form -> Result Form
checkForm env form = case form of
    FormBot -> Right (form, env)
    FormEq t1 t2 -> do
        (checkedT1, env1) <- checkTerm env t1
        (checkedT2, env2) <- checkTerm env1 t2
        if checkedT1 == checkedT2
            then Right (FormEq checkedT1 checkedT2, env2)
            else Left "Equality check failed"
    FormPred pred -> Right (form, env)
    FormAll termId form -> do
        let newEnv = Map.insert (termIdToString termId) (Term termId (Params [])) env
        (checkedForm, newEnv') <- checkForm newEnv form
        Right (FormAll termId checkedForm, newEnv')
    FormSome termId form -> do
        let newEnv = Map.insert (termIdToString termId) (Term termId (Params [])) env
        (checkedForm, newEnv') <- checkForm newEnv form
        Right (FormSome termId checkedForm, newEnv')
    FormNot form -> do
        (checkedForm, newEnv) <- checkForm env form
        Right (FormNot checkedForm, newEnv)
    FormAnd f1 f2 -> do
        (checkedF1, env1) <- checkForm env f1
        (checkedF2, env2) <- checkForm env1 f2
        Right (FormAnd checkedF1 checkedF2, env2)
    FormOr f1 f2 -> do
        (checkedF1, env1) <- checkForm env f1
        (checkedF2, env2) <- checkForm env1 f2
        Right (FormOr checkedF1 checkedF2, env2)
    FormIf f1 f2 -> do
        (checkedF1, env1) <- checkForm env f1
        (checkedF2, env2) <- checkForm env1 f2
        Right (FormIf checkedF1 checkedF2, env2)

checkStep :: Environment -> LineMap -> Integer -> Step -> Result (Step, Environment, LineMap, Integer)
checkStep env lineMap lineNum step = case step of
    StepPrem form -> do
        (checkedForm, newEnv) <- checkForm env form
        let newLineMap = Map.insert lineNum checkedForm lineMap
        Right (StepPrem checkedForm, newEnv, newLineMap, lineNum + 1)
    StepDecConst termId ->
        if Map.member (termIdToString termId) env
            then Left "Constant already declared"
            else Right (step, env, lineMap, lineNum + 1)
    StepDecVar termId ->
        if Map.member (termIdToString termId) env
            then Left "Variable already declared"
            else Right (step, env, lineMap, lineNum + 1)
    StepDecFun termId termIds ->
        if Map.member (termIdToString termId) env
            then Left "Function already declared"
            else Right (step, env, lineMap, lineNum + 1)
    StepAssume form -> do
        (checkedForm, newEnv) <- checkForm env form
        let newLineMap = Map.insert lineNum checkedForm lineMap
        Right (StepAssume checkedForm, newEnv, newLineMap, lineNum + 1)
    StepProof steps -> do
        (checkedSteps, newEnv, newLineMap, newLineNum) <- checkSteps env lineMap lineNum steps
        Right (StepProof checkedSteps, newEnv, newLineMap, newLineNum)
    StepForm ruleId args form -> do
        (checkedForm, newEnv) <- checkForm env form
        let rule = case ruleIdToString ruleId of
                "MP" -> MP
                "AndI" -> AndI
                "AndE1" -> AndE1
                "AndE2" -> AndE2
                _ -> error "Unknown rule"
        case mapM (getFormFromArg lineMap) args of
            Left err -> Left err
            Right forms -> case applyRule rule forms of
                Left err -> Left err
                Right resultForm -> if resultForm == form
                    then Right (StepForm ruleId args checkedForm, newEnv, Map.insert lineNum checkedForm lineMap, lineNum + 1)
                    else Left "Resulting form does not match the expected form"

getFormFromArg :: LineMap -> Arg -> Either String Form
getFormFromArg lineMap (ArgLit line) = case Map.lookup line lineMap of
    Just form -> Right form
    Nothing -> Left $ "No form found at line " ++ show line
getFormFromArg lineMap (ArgSub step) = Left "Sub-steps not supported yet"

checkSequent :: Environment -> Sequent -> Result Sequent
checkSequent env (Seq forms form steps) = do
    (checkedSteps, newEnv, _, _) <- checkSteps env Map.empty 1 steps
    Right (Seq forms form checkedSteps, newEnv)

checkSteps :: Environment -> LineMap -> Integer -> [Step] -> Result ([Step], Environment, LineMap, Integer)
checkSteps env lineMap lineNum steps = foldl (\acc s -> acc >>= \(sts, e, lm, ln) ->
     checkStep e lm ln s >>= \(st, ne, nlm, nln) ->
        Right (st : sts, ne, nlm, nln)) (Right ([], env, lineMap, lineNum)) steps >>= \(sts, e, lm, ln) ->
            Right (reverse sts, e, lm, ln)

isProofCorrect :: Sequent -> Bool
isProofCorrect (Seq _ conclusion steps) =
    case last steps of
        StepForm _ _ form -> form == conclusion
        _ -> False

ruleIdToString :: RuleId -> String
ruleIdToString (RuleId str) = str

termIdToString :: TermId -> String
termIdToString (TermId str) = str
