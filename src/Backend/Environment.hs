module Backend.Environment (
    Env,
    newEnv,
    push,
    addPrem,
    getPrems,
    addRefs,
    getRefs,
    getRef,
    applyRule,
    addConst,
    addVar,
    addFun,
    getConsts,
    getVars,
    getFuns
) where

import qualified Data.Map as Map
import qualified Data.List as List
import Backend.Types

-- Represents the environment for the typechecker.
-- The environment contains:
--   - `prems`: List of premises/assumptions in the current scope.
--   - `refs`: References to labeled steps in the proof.
--   - `rules`: Map of predefined rules.
--   - `consts`: List of declared constants.
--   - `vars`: List of declared variables.
--   - `funs`: List of declared functions.

-- Creates a new environment with an empty state.
newEnv :: Env
newEnv = Env {
    prems = [],
    refs  = Map.empty,
    rules = Map.fromList [
        ("copy", ruleCopy), 
        ("AndI", ruleAndIntro),
        ("AndEL", ruleAndElimLeft),
        ("AndER", ruleAndElimRight),
        ("OrIL", ruleOrIntroLeft),
        ("OrIR", ruleOrIntroRight),
        ("OrE", ruleOrEilm),
        ("IfI", ruleIfIntro),
        ("IfE", ruleIfEilm),
        ("NotI", ruleNotIntro),
        ("NotE", ruleNotEilm),
        ("BotE", ruleBottomElim),
        ("NotNotI", ruleNotNotIntro),
        ("NotNotE", ruleNotNotElim),
        ("MT", ruleMT),
        ("PBC", rulePBC),
        ("LEM", ruleLEM),
        ("EqI", ruleEqI),
        ("EqE", ruleEqE),
        ("AllE", ruleAllE),
        ("AllI", ruleAllI),
        ("SomeE", ruleSomeE),
        ("SomeI", ruleSomeI)
    ],
    consts = [],
    vars = [],
    funs = [],
    pos  = []
}

-- Pushes a new context to the environment (used when entering a subproof or box).
-- This resets the list of premises for the new scope.
push :: Env -> Env
push env = env { prems = [] }

-- Section: Adders

-- Adds a premise/assumption to the environment.
-- Premises are logical formulas that are assumed to be true in the current scope.
addPrem :: Env -> Formula -> Env
addPrem env prem = env { prems = prems env ++ [prem] }

-- Adds a constant to the environment.
-- Constants are terms with no arguments.
addConst :: Env -> String -> Env
addConst env id = 
    let constTerm = Term id []
    in env { consts = constTerm : consts env }

-- Adds a variable to the environment.
-- Variables are placeholders that can be used in formulas or terms.
addVar :: Env -> String -> Env
addVar env id = 
    let varTerm = Term id []
    in env { vars = varTerm : vars env }

-- Adds a function to the environment.
-- Functions are terms with arguments.
addFun :: Env -> String -> [String] -> Env
addFun env id args = 
    let funTerm = Term id (map (\arg -> Term arg []) args)
    in env { funs = funTerm : funs env }

-- Adds references to the environment.
-- References map labels to arguments (e.g., proofs, formulas, or terms).
addRefs :: Env -> [Ref] -> Arg -> Env
addRefs env labels form = env { refs = Map.union (refs env) (Map.fromList [(label, form) | label <- labels]) }

-- Section: Getters

-- Retrieves all premises/assumptions in the current scope.
getPrems :: Env -> [Formula]
getPrems env = prems env

-- Retrieves all constants in the environment.
getConsts :: Env -> [Term]
getConsts env = consts env

-- Retrieves all variables in the environment.
getVars :: Env -> [Term]
getVars env = vars env

-- Retrieves all functions in the environment.
getFuns :: Env -> [Term]
getFuns env = funs env

-- Retrieves the values corresponding to a list of references.
-- Returns an error if any reference is invalid.
getRefs :: Env -> [Ref] -> Result [Arg]
getRefs _ [] = Ok [] []
getRefs env (x : xs) = 
    case getRefs env xs of
        Error warns kind msg -> Error warns kind msg
        Ok warns1 args -> 
            case getRef env x of 
                Error warns kind msg -> Error (warns++warns1) kind msg
                Ok warns2 arg -> Ok (warns1++warns2) (arg : args)

-- Retrieves the value corresponding to a single reference.
-- Returns an error if the reference does not exist.
getRef :: Env -> Ref -> Result Arg
getRef env ref = 
    case Map.lookup ref (refs env) of
        Nothing -> Error [] TypeError ("No ref " ++ show ref ++ " exists.") 
        Just arg -> Ok [] arg

-- Applies a rule to a list of arguments and checks the result.
-- If the rule application succeeds, the resulting formula is returned.
-- Otherwise, an error is returned.
applyRule :: Env -> String -> [Arg] -> Formula -> Result Formula
applyRule env name args res = 
    case Map.lookup name (rules env) of
        Nothing -> Error [] TypeError ("No rule named " ++ name ++ " exists.") 
        Just rule -> 
            case rule env args res of 
                Error warns kind msg -> Error warns kind ("While applying rule " ++ name ++ ": " ++ msg)
                Ok warns res_t -> 
                    if res_t == res then Ok warns res_t 
                    else Error warns TypeError ("Wrong conclusion when using rule " ++ name ++ ": expected " ++ show res_t ++ ", got " ++ show res)

-- All predefined rules.
-- Rules are functions that take:
--   - A list of arguments (e.g., proofs, formulas, or terms).
--   - An expected result formula.
--   - Return either the resulting formula or an error.

ruleCopy :: Env -> [Arg] -> Formula -> Result Formula
ruleCopy _ [ArgForm form] _ = Ok [] form
ruleCopy env forms          _ = Error [] TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++ ".")

ruleAndIntro :: Env -> [Arg] -> Formula -> Result Formula
ruleAndIntro _ [ArgForm a, ArgForm b] _ = Ok [] (And a b) 
ruleAndIntro env [_, ArgForm _]         _ = Error [] TypeError "Argument 1 needs to be a formula."
ruleAndIntro env [_, _]                 _ = Error [] TypeError "Argument 2 needs to be a formula."
ruleAndIntro env forms                  _ = Error [] TypeError ("Rule takes 2 arguments not " ++ show (List.length forms) ++ ".")

ruleAndElimLeft :: Env -> [Arg] -> Formula -> Result Formula
ruleAndElimLeft _ [ArgForm (And l _)] _ = Ok [] l
ruleAndElimLeft env [ArgForm _]         _ = Error [] TypeError "Argument needs to be an and formula."
ruleAndElimLeft env [_]                 _ = Error [] TypeError "Argument needs to be a formula."
ruleAndElimLeft env forms               _ = Error [] TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++ ".")

ruleAndElimRight :: Env -> [Arg] -> Formula -> Result Formula
ruleAndElimRight _ [ArgForm (And _ r)] _ = Ok [] r
ruleAndElimRight env [ArgForm _]         _ = Error [] TypeError "Argument needs to be an and formula."
ruleAndElimRight env [_]                 _ = Error [] TypeError "Argument needs to be a formula."
ruleAndElimRight env forms               _ = Error [] TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++ ".")

ruleOrIntroLeft :: Env -> [Arg] -> Formula -> Result Formula
ruleOrIntroLeft env [ArgForm a]  r@(Or b _) = if a == b then Ok [] r else Error [] TypeError "Argument 1 did not match left hand side of or."
ruleOrIntroLeft env [ArgForm _]           _ = Error [] TypeError "Conclusion needs to be an or formula."
ruleOrIntroLeft env [_]            (Or _ _) = Error [] TypeError "Argument needs to be a formula."
ruleOrIntroLeft env forms                 _ = Error [] TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++ ".")

ruleOrIntroRight :: Env -> [Arg] -> Formula -> Result Formula
ruleOrIntroRight env [ArgForm a]  r@(Or _ b) = if a == b then Ok [] r else Error [] TypeError "Argument 1 did not match right hand side of or."
ruleOrIntroRight env [ArgForm _]           _ = Error [] TypeError "Conclusion needs to be an or formula."
ruleOrIntroRight env [_]            (Or _ _) = Error [] TypeError "Argument needs to be a formula."
ruleOrIntroRight env forms                 _ = Error [] TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++ ".")

ruleOrEilm :: Env -> [Arg] -> Formula -> Result Formula
ruleOrEilm env [ArgForm (Or a b), ArgProof (Proof [p1] c1), ArgProof (Proof [p2] c2)] _ = 
    if a == p1 then
        if b == p2 then 
            if c1 == c2 then Ok [] c1 else Error [] TypeError "The conclusions of the two proofs did not match."
        else Error [] TypeError "The premise of the second proof did not match the right hand side of the or statement."
    else Error [] TypeError "The premise of the first proof did not match the left hand side of the or statement."
ruleOrEilm env [ArgForm (Or _ _), ArgProof _, _] _ = Error [] TypeError "Argument 3 needs to be a proof."
ruleOrEilm env [ArgForm (Or _ _), _, _]          _ = Error [] TypeError "Argument 2 needs to be a proof."
ruleOrEilm env [ArgForm _, _, _]                 _ = Error [] TypeError "Argument 1 needs to be an or formula."
ruleOrEilm env [_, _, _]                         _ = Error [] TypeError "Argument 1 needs to be a formula."
ruleOrEilm env forms                             _ = Error [] TypeError ("Rule takes 3 arguments not " ++ show (List.length forms) ++ ".")

ruleIfIntro :: Env -> [Arg] -> Formula -> Result Formula
ruleIfIntro _ [ArgProof (Proof [a] b)] _ = Ok [] (If a b)
ruleIfIntro env [_]                      _ = Error [] TypeError "Argument 1 needs to be a proof."
ruleIfIntro env forms                    _ = Error [] TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++ ".")

ruleIfEilm :: Env -> [Arg] -> Formula -> Result Formula
ruleIfEilm env [ArgForm a, ArgForm (If b c)] r = 
    if a == b then 
        if c == r then Ok [] r else Error [] TypeError "Conclusions did not match."
    else Error [] TypeError "Premise did not match argument 1."
ruleIfEilm env [ArgForm _, ArgForm _] _ = Error [] TypeError "Argument 2 needs to be an if-then formula."
ruleIfEilm env [ArgForm _, _]         _ = Error [] TypeError "Argument 2 needs to be a formula."
ruleIfEilm env [_        , _]         _ = Error [] TypeError "Argument 1 needs to be a formula."
ruleIfEilm env forms                  _ = Error [] TypeError ("Rule takes 2 arguments not " ++ show (List.length forms) ++ ".")

ruleNotIntro :: Env -> [Arg] -> Formula -> Result Formula
ruleNotIntro _ [ArgProof (Proof [a] Bot)] _ = Ok [] (Not a)
ruleNotIntro env [ArgProof (Proof [_] _ )]  _ = Error [] TypeError "Argument 1 needs to be a proof with the conclusion of bot."
ruleNotIntro env [_]                        _ = Error [] TypeError "Argument 1 needs to be a proof."
ruleNotIntro env forms                      _ = Error [] TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++ ".")

ruleNotEilm :: Env -> [Arg] -> Formula -> Result Formula
ruleNotEilm env [ArgForm a, ArgForm (Not b)] _ = if a == b then Ok [] Bot else Error [] TypeError "Argument 2 is not the negation of argument 1."
ruleNotEilm env [ArgForm _, ArgForm _]       _ = Error [] TypeError "Argument 2 needs to be a not formula."
ruleNotEilm env [ArgForm _, _]               _ = Error [] TypeError "Argument 2 needs to be a formula."
ruleNotEilm env [_, _]                       _ = Error [] TypeError "Argument 1 needs to be a formula."
ruleNotEilm env forms                        _ = Error [] TypeError ("Rule takes 2 arguments not " ++ show (List.length forms) ++ ".")

ruleBottomElim :: Env -> [Arg] -> Formula -> Result Formula
ruleBottomElim _ [ArgForm Bot] r = Ok [] r
ruleBottomElim env [ArgForm _]   _ = Error [] TypeError "Argument 1 needs to be a bottom formula."
ruleBottomElim env [_]           _ = Error [] TypeError "Argument 1 needs to be a formula."
ruleBottomElim env forms         _ = Error [] TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++ ".")

ruleNotNotIntro :: Env -> [Arg] -> Formula -> Result Formula
ruleNotNotIntro _ [ArgForm a] _ = Ok [] (Not (Not a))
ruleNotNotIntro env [_]         _ = Error [] TypeError "Argument 1 needs to be a formula."
ruleNotNotIntro env forms       _ = Error [] TypeError ("Not not introduction takes 1 argument not " ++ show (List.length forms) ++ ".")

ruleNotNotElim :: Env -> [Arg] -> Formula -> Result Formula
ruleNotNotElim _ [ArgForm (Not (Not a))] _ = Ok [] a
ruleNotNotElim env [ArgForm _]             _ = Error [] TypeError "Argument 1 needs to be a not not formula."
ruleNotNotElim env [_]                     _ = Error [] TypeError "Argument 1 needs to be a formula."
ruleNotNotElim env forms                   _ = Error [] TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++ ".")

ruleMT :: Env -> [Arg] -> Formula -> Result Formula
ruleMT env [ArgForm (If a b), ArgForm (Not c)] _ = if b == c then Ok [] (Not a) else Error [] TypeError "Conclusion in argument 1 did not match argument 2."
ruleMT env [ArgForm (If _ _), ArgForm _]       _ = Error [] TypeError "Argument 2 needs to be a not formula."
ruleMT env [ArgForm (If _ _), _]               _ = Error [] TypeError "Argument 2 needs to be a formula."
ruleMT env [ArgForm _, _ ]                     _ = Error [] TypeError "Argument 1 needs to be an if-then formula."
ruleMT env [_, _]                              _ = Error [] TypeError "Argument 1 needs to be a formula."
ruleMT env forms                               _ = Error [] TypeError ("Rule takes 2 arguments not " ++ show (List.length forms) ++ ".")

rulePBC :: Env -> [Arg] -> Formula -> Result Formula
rulePBC _ [ArgProof (Proof [Not a] Bot)] _ = Ok [] a
rulePBC env [ArgProof (Proof [Not _] _)]   _ = Error [] TypeError "Conclusion in argument 1 needs to be a bot formula."
rulePBC env [ArgProof (Proof [_] _)]       _ = Error [] TypeError "Premise in argument 1 needs to be a not formula."
rulePBC env [_]                            _ = Error [] TypeError "Argument 1 needs to be a proof."
rulePBC env forms                          _ = Error [] TypeError ("Rule elimination takes 1 argument not " ++ show (List.length forms) ++ ".")

ruleLEM :: Env -> [Arg] -> Formula -> Result Formula
ruleLEM env [] r@(Or a (Not b)) = if a == b then Ok [] r else Error [] TypeError "Right hand side is not the negation of the left hand side."
ruleLEM env []                _ = Error [] TypeError "The conclusion must be an or statement."
ruleLEM env forms             _ = Error [] TypeError ("Rule takes 0 arguments not " ++ show (List.length forms) ++ ".")

ruleEqI :: Env -> [Arg] -> Formula -> Result Formula
ruleEqI env [] r@(Eq a b) = if a == b then Ok [] r else Error [] TypeError "Left and right hand side are not the same."
ruleEqI env [] _  = Error [] TypeError "The conclusion must be an eq statement."
ruleEqI env forms _ = Error [] TypeError ("Rule takes 0 arguments not " ++ show (List.length forms) ++ ".")

ruleEqE:: Env-> [Arg] -> Formula -> Result Formula
ruleEqE env _ _ = Error [] UnknownError "Unimplemented."

ruleAllE:: Env-> [Arg] -> Formula -> Result Formula
ruleAllE env _ _ = Error [] UnknownError "Unimplemented."

ruleAllI:: Env-> [Arg] -> Formula -> Result Formula
ruleAllI env _ _ = Error [] UnknownError "Unimplemented."

ruleSomeE:: Env-> [Arg] -> Formula -> Result Formula
ruleSomeE env _ _ = Error [] UnknownError "Unimplemented."

ruleSomeI:: Env-> [Arg] -> Formula -> Result Formula
ruleSomeI env _ _ = Error [] UnknownError "Unimplemented."