module Backend.Environment (
    Env,
    newEnv,
    push,
    addPrem,
    getPrems,
    addRefs,
    getRefs,
    getRef,
    applyRule
) where

import qualified Data.Map as Map
import qualified Data.List as List
import Backend.Types

{-
    Type containing all environment information for an instace of the typechecker.
    -prems: Contains a list of all premises/assumptions in the current scope in the order they were introduced
    -refs:  References to all labeld steps in the proof. 
    -rules: All rules 
-}
data Env =  Env {
    prems :: [Formula],
    refs  :: Map.Map Ref Arg,
    rules :: Map.Map String (Env->[Arg]->Formula->Result Formula)
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
addRefs :: Env -> [Ref] -> Arg -> Env
addRefs env labels form = env{refs = Map.union (refs env) (Map.fromList [(label, form)| label <-labels])}

{-
    Returns the values corresponding to the refrenses in an list
    -params:
        - Environment
        - List of labels
    -return: List of values associated with the labels
-}
getRefs :: Env -> [Ref] -> Result [Arg]
getRefs _ [] = Ok []
getRefs env (x: xs) = case getRefs env xs of
    Error kind msg -> Error kind msg
    Ok args -> case getRef env x of 
        Error kind msg -> Error kind msg
        Ok arg -> Ok ([arg] ++ args)

getRef :: Env -> Ref -> Result Arg
getRef env ref = case Map.lookup ref (refs env) of
    Nothing -> Error TypeError ("No ref " ++ show ref ++ " exists.") 
    Just arg -> Ok arg
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
    Just rule -> case rule env args res of 
            Error kind msg -> Error kind  ("While applying rule " ++ name ++ ": " ++ msg)
            Ok res_t -> if res_t == res then Ok res_t 
            else Error TypeError ("Wrong conclusion when using rule " ++ name ++ " expected " ++ show res_t ++ " not " ++ show res) 


{-
    All predefined rules
    Rules are functions on the form
    -params: 
        -Environment
        -List of arguments
        -Expected result of rule
    -return: result of rule
-}


ruleCopy:: Env-> [Arg] -> Formula -> Result Formula
ruleCopy _ [ArgForm form] _ = Ok form
ruleCopy _ forms          _ = Error TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++".")

ruleAndIntro:: Env-> [Arg] -> Formula -> Result Formula
ruleAndIntro _ [ArgForm a, ArgForm b] _ = Ok (And a b) 
ruleAndIntro _ [_, ArgForm _]         _ = Error TypeError "Argument 1 needs to be a formula."
ruleAndIntro _ [_, _]                 _ = Error TypeError "Argument 2 needs to be a formula."
ruleAndIntro _ forms                  _  = Error TypeError ("Rule takes 2 arguments not " ++ show (List.length forms) ++".")

ruleAndElimLeft:: Env->  [Arg] -> Formula -> Result Formula
ruleAndElimLeft _ [ArgForm (And l _)] _ = Ok l
ruleAndElimLeft _ [ArgForm _]         _ = Error TypeError "Argument needs to be a and formula."
ruleAndElimLeft _ [_]                 _ = Error TypeError "Argument needs to be a formula."
ruleAndElimLeft _ forms               _  = Error TypeError ("Rule takes 1 arguments not " ++ show (List.length forms) ++".")

ruleAndElimRight:: Env-> [Arg] -> Formula -> Result Formula
ruleAndElimRight _ [ArgForm (And _ r)] _ = Ok r
ruleAndElimRight _ [ArgForm _]         _ = Error TypeError "Argument needs to be a and formula."
ruleAndElimRight _ [_]                 _ = Error TypeError "Argument needs to be a formula."
ruleAndElimRight _ forms               _ = Error TypeError ("Rule takes 1 arguments not " ++ show (List.length forms) ++".")

ruleOrIntroLeft:: Env-> [Arg] -> Formula -> Result Formula
ruleOrIntroLeft _ [ArgForm a]  r@(Or b _) = if a == b then Ok r else Error TypeError "Argument 1 did not match left hand side of or."
ruleOrIntroLeft _ [ArgForm _]           _ = Error TypeError "Conclusion needs to be a or formula."
ruleOrIntroLeft _ [_]            (Or _ _) = Error TypeError "Argument needs to be a formula."
ruleOrIntroLeft _ forms                 _ = Error TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++".")

ruleOrIntroRight:: Env-> [Arg] -> Formula -> Result Formula
ruleOrIntroRight _ [ArgForm a]  r@(Or _ b) = if a == b then Ok r else Error TypeError "Argument 1 did not match right hand side of or."
ruleOrIntroRight _ [ArgForm _]           _ = Error TypeError "Conclusion needs to be a or formula."
ruleOrIntroRight _ [_]            (Or _ _) = Error TypeError "Argument needs to be a formula."
ruleOrIntroRight _ forms                 _ = Error TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++".")

ruleOrEilm:: Env-> [Arg] -> Formula -> Result Formula
ruleOrEilm _ [ArgForm (Or a b), ArgProof (Proof [p1] c1), ArgProof (Proof [p2] c2)] _ = if a == p1 then
    if b == p2 then 
        if c1 == c2 
            then Ok c1 
            else Error TypeError "The conclusions of the two proofs did not match."
        else Error TypeError "The premise of the second proof did not match the right hand side of the or statement."
    else Error TypeError "The premise of the second proof did not match the left hand side of the or statement."
ruleOrEilm _ [ArgForm (Or _ _), ArgProof _, _] _ = Error TypeError  "Argument 3 need to be a proof."
ruleOrEilm _ [ArgForm (Or _ _), _, _]          _ = Error TypeError  "Argument 2 need to be a proof."
ruleOrEilm _ [ArgForm _, _, _]                 _ = Error TypeError  "Argument 1 need to be a or formula."
ruleOrEilm _ [_, _, _]                         _ = Error TypeError  "Argument 1 need to be a formula."
ruleOrEilm _ forms                             _ = Error TypeError ("Rule takes 3 argument not " ++ show (List.length forms) ++".")

ruleIfIntro:: Env-> [Arg] -> Formula -> Result Formula
ruleIfIntro _ [ArgProof (Proof [a] b)] _ = Ok (If a b)
ruleIfIntro _ [_]                      _ = Error TypeError "Argument 1 need to be a proof."
ruleIfIntro _ forms                    _ = Error TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++".")

ruleIfEilm:: Env-> [Arg] -> Formula -> Result Formula
ruleIfEilm _ [ArgForm a, ArgForm (If b c)] r = if a == b then 
    if c == r 
        then Ok r
        else Error TypeError "Conclusions did not match"
    else Error TypeError "Premise did not match argument 1"
ruleIfEilm _ [ArgForm _, ArgForm _] _ = Error TypeError "Argument 2 need to be a if then formula."
ruleIfEilm _ [ArgForm _, _]         _ = Error TypeError "Argument 2 need to be a formula."
ruleIfEilm _ [_        , _]         _ = Error TypeError "Argument 1 need to be a formula."
ruleIfEilm _ forms                  _ = Error TypeError ("Rule takes 2 argument not " ++ show (List.length forms) ++".")

ruleNotIntro:: Env-> [Arg] -> Formula -> Result Formula
ruleNotIntro _ [ArgProof (Proof [a] Bot)] _ = Ok (Not a)
ruleNotIntro _ [ArgProof (Proof [_] _ )]  _ = Error TypeError "Argument 1 need to be a proof with the conclusion of bot."
ruleNotIntro _ [_]                        _ = Error TypeError "Argument 1 need to be a proof."
ruleNotIntro _ forms                      _  = Error TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++".")
ruleNotEilm:: Env-> [Arg] -> Formula -> Result Formula
ruleNotEilm _ [ArgForm a, ArgForm (Not b)] _ = if a == b then Ok Bot else Error TypeError "Argument 2 is not the negation of argument 1."
ruleNotEilm _ [ArgForm _, ArgForm _]       _ = Error TypeError "Argument 2 need to be a not formula."
ruleNotEilm _ [ArgForm _, _]               _ = Error TypeError "Argument 2 need to be a formula."
ruleNotEilm _ [_, _]                       _ = Error TypeError "Argument 1 need to be a formula."
ruleNotEilm _ forms                        _  = Error TypeError ("Rule takes 2 argument not " ++ show (List.length forms) ++".")

ruleBottomElim:: Env-> [Arg] -> Formula -> Result Formula
ruleBottomElim _ [ArgForm Bot] r = Ok r
ruleBottomElim _ [ArgForm _]   _ = Error TypeError "Argument 1 needs to be a bottom formula."
ruleBottomElim _ [_]           _ = Error TypeError "Argument 1 needs to be a formula."
ruleBottomElim _ forms         _ = Error TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++".")

ruleNotNotIntro:: Env-> [Arg] -> Formula -> Result Formula
ruleNotNotIntro _ [ArgForm a] _ = Ok (Not (Not a))
ruleNotNotIntro _ [_]         _ = Error TypeError "Argument 1 needs to be a formula."
ruleNotNotIntro _ forms       _  = Error TypeError ("Not not introduction takes 1 argument not " ++ show (List.length forms) ++".")
ruleNotNotElim:: Env-> [Arg] -> Formula -> Result Formula
ruleNotNotElim _ [ArgForm (Not (Not a))] _ = Ok a
ruleNotNotElim _ [ArgForm _]             _ = Error TypeError "Argument 1 needs to be a not not formula"
ruleNotNotElim _ [_]                     _ = Error TypeError "Argument 1 needs to be a formula"
ruleNotNotElim _ forms                   _ = Error TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++".")

ruleMT:: Env-> [Arg] -> Formula -> Result Formula
ruleMT _ [ArgForm (If a b), ArgForm (Not c)] _ = if b == c then Ok (Not a) else Error TypeError "Conclusion in argument 1 did not match argument 2"
ruleMT _ [ArgForm (If _ _), ArgForm _]       _ = Error TypeError "Argument 2 need to be a not formula."
ruleMT _ [ArgForm (If _ _), _]               _ = Error TypeError "Argument 2 need to be a formula."
ruleMT _ [ArgForm _, _ ]                     _ = Error TypeError "Argument 1 need to be a if then formula."
ruleMT _ [_, _]                              _ = Error TypeError "Argument 1 need to be a formula."
ruleMT _ forms                               _  = Error TypeError ("Rule takes 2 argument not " ++ show (List.length forms) ++".")

rulePBC:: Env-> [Arg] -> Formula -> Result Formula
rulePBC _ [ArgProof (Proof [Not a] Bot)] _ = Ok a
rulePBC _ [ArgProof (Proof [Not _] _)]   _ = Error TypeError "Conclusion in argument 1 need to be bot formula."
rulePBC _ [ArgProof (Proof [_] _)]       _ = Error TypeError "Premise in argument 1 need to be not formula."
rulePBC _ [_]                            _ = Error TypeError "Argument 1 need to be a proof."
rulePBC _ forms                          _  = Error TypeError ("Rule eliminaton takes 1 argument not " ++ show (List.length forms) ++".")

ruleLEM:: Env-> [Arg] -> Formula -> Result Formula
ruleLEM _ [] r@(Or a (Not b)) = if a == b then Ok r else Error TypeError "Right hand side is not the negation of the left hand side."
ruleLEM _ []                _ = Error TypeError "The conclusion of must be an or statement."
ruleLEM _ forms             _ = Error TypeError ("Rule takes 0 argument not " ++ show (List.length forms) ++".")

ruleEqI:: Env-> [Arg] -> Formula -> Result Formula
ruleEqI _ [] r@(Eq a b) = if a == b then Ok r else Error TypeError "Left and right hand side is not the same."
ruleEqI _ [] _  = Error TypeError "The conclusion of must be an eq statement."
ruleEqI _ forms _ = Error TypeError ("Rule takes 0 argument not " ++ show (List.length forms) ++".")

ruleEqE:: Env-> [Arg] -> Formula -> Result Formula
ruleEqE _ _ _ = Error UnknownError "Unimplemented."

ruleAllE:: Env-> [Arg] -> Formula -> Result Formula
ruleAllE _ _ _ = Error UnknownError "Unimplemented."

ruleAllI:: Env-> [Arg] -> Formula -> Result Formula
ruleAllI _ _ _ = Error UnknownError "Unimplemented."

ruleSomeE:: Env-> [Arg] -> Formula -> Result Formula
ruleSomeE _ _ _ = Error UnknownError "Unimplemented."

ruleSomeI:: Env-> [Arg] -> Formula -> Result Formula
ruleSomeI _ _ _ = Error UnknownError "Unimplemented."