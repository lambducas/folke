module Backend.Environment (
    Env,
    newEnv,
    push,
    addPrem,
    getPrems,
    addRefs,
    getRefs,
    getRef,
    pushPos,
    applyRule,
    addConst,
    addVar,
    getConsts,
    getVars
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

newEnv :: Env
newEnv = Env {
    prems = [],
    refs  = Map.empty,
    rules = Map.fromList [
        ("copy", ruleCopy),
        ("COPY", ruleCopy),
        ("C", ruleCopy),
        ("REITERATION", ruleCopy),
        ("R", ruleCopy),

        ("AndI", ruleAndIntro),
        ("&I", ruleAndIntro),
        ("∧I", ruleAndIntro),

        ("AndEL", ruleAndElimLeft),
        ("&EL", ruleAndElimLeft),
        ("∧EL", ruleAndElimLeft),

        ("AndER", ruleAndElimRight),
        ("&ER", ruleAndElimRight),
        ("∧ER", ruleAndElimRight),

        ("OrIL", ruleOrIntroLeft),
        ("|IL", ruleOrIntroLeft),
        ("∨IL", ruleOrIntroLeft),

        ("OrIR", ruleOrIntroRight),
        ("|IR", ruleOrIntroRight),
        ("∨IR", ruleOrIntroRight),

        ("OrE", ruleOrEilm),
        ("|E", ruleOrEilm),
        ("∨E", ruleOrEilm),

        ("ImplI", ruleImplIntro),
        ("IfI", ruleImplIntro),
        ("->I", ruleImplIntro),
        ("→I", ruleImplIntro),

        ("ImplE", ruleImplEilm),
        ("IfE", ruleImplEilm),
        ("->E", ruleImplEilm),
        ("→E", ruleImplEilm),

        ("ImplI", ruleImplIntro),
        ("IfI", ruleImplIntro),
        ("->I", ruleImplIntro),
        ("→I", ruleImplIntro),

        ("ImplE", ruleImplEilm),
        ("IfE", ruleImplEilm),
        ("->E", ruleImplEilm),
        ("→E", ruleImplEilm),

        ("ImplI", ruleImplIntro),
        ("IfI", ruleImplIntro),
        ("->I", ruleImplIntro),
        ("→I", ruleImplIntro),

        ("ImplE", ruleImplEilm),
        ("IfE", ruleImplEilm),
        ("->E", ruleImplEilm),
        ("→E", ruleImplEilm),

        ("ImplI", ruleImplIntro),
        ("->I", ruleImplIntro),
        ("→I", ruleImplIntro),

        ("ImplE", ruleImplEilm),
        ("IfE", ruleImplEilm),
        ("->E", ruleImplEilm),
        ("→E", ruleImplEilm),

        ("NotI", ruleNotIntro),
        ("!I", ruleNotIntro),
        ("¬I", ruleNotIntro),

        ("NotE", ruleNotEilm),
        ("!E", ruleNotEilm),
        ("¬E", ruleNotEilm),

        ("BotE", ruleBottomElim),
        ("botE", ruleBottomElim),
        ("#E", ruleBottomElim),
        ("⊥E", ruleBottomElim),

        ("NotNotI", ruleNotNotIntro),
        ("!!I", ruleNotNotIntro),
        ("¬¬I", ruleNotNotIntro),

        ("NotNotE", ruleNotNotElim),
        ("!!E", ruleNotNotElim),
        ("¬¬E", ruleNotNotElim),

        ("MT", ruleMT),

        ("PBC", rulePBC),

        ("LEM", ruleLEM),

        ("EqI", ruleEqI),
        ("=I", ruleEqI),

        ("EqE", ruleEqE),
        ("=E", ruleEqE),

        ("AllE", ruleAllE),
        ("forallE", ruleAllE),
        ("∀E", ruleAllE),

        ("AllI", ruleAllI),
        ("forallI", ruleAllI),
        ("∀I", ruleAllI),

        ("SomeE", ruleSomeE),
        ("existsE", ruleSomeE),
        ("∃E", ruleSomeE),

        ("SomeI", ruleSomeI),
        ("existsI", ruleSomeI),
        ("∃I", ruleSomeI)
    ],
    consts = [],
    vars = [],
    pos  = [],
    rule = ""
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
addConst :: Env -> Term -> Result Env
addConst env c = Ok [] env { consts = c : consts env } -- TODO check if const overides another constanct?

-- Adds a variable to the environment.
-- Variables are placeholders that can be used in formulas or terms.
addVar :: Env -> Term -> Result Env
addVar env v = Ok []  env { vars = v : vars env } -- TODO check if var overides another variable?

-- Adds references to the environment.
-- References map labels to arguments (e.g., proofs, formulas, or terms).
addRefs :: Env -> [Ref] -> Arg -> Env
addRefs env labels form = env { refs = Map.union (refs env) (Map.fromList [(label, (0, form)) | label <- labels]) }

-- Section: Getters

-- Retrieves all premises/assumptions in the current scope.
getPrems :: Env -> [Formula]
getPrems = prems

-- Retrieves all constants in the environment.
getConsts :: Env -> [Term]
getConsts = consts

-- Retrieves all variables in the environment.
getVars :: Env -> [Term]
getVars = vars

-- Retrieves the values corresponding to a list of references.
-- Returns an error if any reference is invalid.
getRefs :: Env -> [Ref] -> Result (Env, [Arg])
getRefs env [] = Ok [] (env, [])
getRefs env (x : xs) =
    case getRefs env xs of
        Error warns env err -> Error warns env err
        Ok warns1 (env1, args) ->
            case getRef env1 x of
                Error warns env err -> Error (warns++warns1) env err
                Ok warns2 (env2, arg) -> Ok (warns1++warns2) (env2, arg : args)

-- Retrieves the value corresponding to a single reference.
-- Returns an error if the reference does not exist.
getRef :: Env -> Ref -> Result (Env, Arg)
getRef env ref =
    case Map.lookup ref (refs env) of
        Nothing -> Error [] env (TypeError ("No ref " ++ show ref ++ " exists."))
        Just (count, arg) -> Ok [] (env{refs = Map.insert ref (count+1, arg) (refs env) }, arg)

pushPos :: Env -> [Ref] -> Env
pushPos env r = env {pos = r ++ pos env}
-- Applies a rule to a list of arguments and checks the result.
-- If the rule application succeeds, the resulting formula is returned.
-- Otherwise, an error is returned.
applyRule :: Env -> String -> [Arg] -> Formula -> Result Formula
applyRule env name args res =
    case Map.lookup name (rules env) of
        Nothing -> Error [] env (RuleNotFoundError name)
        Just rule_f ->
            case rule_f (env{rule = name}) (zip [1..] args) res of
                Error warns env err -> Error warns env err
                Ok warns res_t ->
                    if res_t == res then Ok warns res_t
                    else Error warns env (RuleConcError ("Wrong conclusion when using rule, expected " ++ show res_t ++ ", got " ++ show res))

-- All predefined rules.
-- Rules are functions that take:
--   - A list of arguments (e.g., proofs, formulas, or terms).
--   - An expected result formula.
--   - Return either the resulting formula or an error.

ruleCopy :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleCopy _ [(_, ArgForm form)] _ = Ok [] form
ruleCopy env forms          _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleAndIntro :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleAndIntro _ [(_, ArgForm a), (_, ArgForm b)] _ = Ok [] (And a b)
ruleAndIntro env [_, (j, ArgForm _)]         _ = Error [] env (RuleArgError j "Needs to be a formula.")
ruleAndIntro env [(i, _), _]                 _ = Error [] env (RuleArgError i "Needs to be a formula.")
ruleAndIntro env forms                  _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 2 )

ruleAndElimLeft :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleAndElimLeft _ [(_ ,ArgForm (And l _))] _ = Ok [] l
ruleAndElimLeft env [_]                 _ = Error [] env (RuleArgError 1 "Needs to be a and formula.")
ruleAndElimLeft env forms               _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleAndElimRight :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleAndElimRight _ [(_, ArgForm (And _ r))] _ = Ok [] r
ruleAndElimRight env [_]                 _ = Error [] env (RuleArgError 1 "Needs to be a and formula.")
ruleAndElimRight env forms               _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleOrIntroLeft :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleOrIntroLeft env [(_, ArgForm a)]  r@(Or b _) = if a == b then Ok [] r else Error [] env (RuleArgError 1 "Did not match left hand side of conclusion.")
ruleOrIntroLeft env [(_, ArgForm _)]           _ = Error [] env (RuleConcError "Conclusion needs to be an or formula.")
ruleOrIntroLeft env [_]            (Or _ _) = Error [] env (RuleArgError 1 "Needs to be a and formula.")
ruleOrIntroLeft env forms                 _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleOrIntroRight :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleOrIntroRight env [(_, ArgForm a)]  r@(Or _ b) = if a == b then Ok [] r else Error [] env (RuleArgError 1 "did not match right hand side of conclusion.")
ruleOrIntroRight env [(_, ArgForm _)]           _ = Error [] env (RuleConcError "Conclusion needs to be an or formula.")
ruleOrIntroRight env [_]            (Or _ _) = Error [] env (RuleArgError 1 "needs to be a and formula.")
ruleOrIntroRight env forms                 _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleOrEilm :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleOrEilm env [(_, ArgForm (Or a b)), (j, ArgProof (Proof _ [p1] c1)), (k, ArgProof (Proof _ [p2] c2))] _ =
    if a == p1 then
        if b == p2 then
            if c1 == c2 then Ok [] c1 else Error [] env (RuleConcError "The conclusions of the two proofs did not match.")--Not realy conclusion error? 
        else Error [] env (RuleArgError k "The premise of the proof did not match the right hand side of the or statement.")
    else Error [] env (RuleArgError j "The premise of the proof did not match the left hand side of the or statement.")
ruleOrEilm env [b@(_, ArgProof _), a@(_, ArgForm _), c@(_, ArgProof _)] r = ruleOrEilm env [a, b, c] r
ruleOrEilm env [b@(_, ArgProof _), c@(_, ArgProof _), a@(_, ArgForm _)] r = ruleOrEilm env [a, b, c] r
ruleOrEilm env [(_, ArgForm (Or _ _)), (_, ArgProof _), (k, _)] _ = Error [] env (RuleArgError k "Needs to be an proof.")
ruleOrEilm env [(_, ArgForm (Or _ _)), (j, _), _]          _ = Error [] env (RuleArgError j "Needs to be an proof.")
ruleOrEilm env [(i, _), _, _]                         _ = Error [] env (RuleArgError i "Needs to be an or formula.")
ruleOrEilm env forms                             _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 3 )

ruleImplIntro :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleImplIntro _ [(_, ArgProof (Proof _ [a] b))] _ = Ok [] (Impl a b)
ruleImplIntro env [_]                      _ = Error [] env (RuleArgError 1 "Needs to be an proof.")
ruleImplIntro env forms                    _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleImplEilm :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleImplEilm env [(_, ArgForm a), (j, ArgForm (Impl b c))] r =
    if a == b then
        if c == r then Ok [] r else Error [] env (RuleConcError "Conclusions did not match.")
    else Error [] env (RuleArgError j "Premise did not match argument 1.")
ruleImplEilm env [b@(_, ArgForm (Impl _ _)), a@(_, ArgForm _)] r = ruleImplEilm env [a,b] r
ruleImplEilm env [(_, ArgForm _), (j, _)]         _ = Error [] env (RuleArgError j "Needs to be an implication formula.")
ruleImplEilm env [(i, _)        , _]         _ = Error [] env (RuleArgError i "Needs to be a formula.")
ruleImplEilm env forms                  _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 2 )

ruleNotIntro :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleNotIntro _ [(_, ArgProof (Proof _ [a] Bot))] _ = Ok [] (Not a)
ruleNotIntro env [_]                        _ = Error [] env (RuleArgError 1 "Needs to be a proof with the conclusion of bot.")
ruleNotIntro env forms                      _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleNotEilm :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleNotEilm env [(i, ArgForm a), (j, ArgForm (Not b))] _ = if a == b then Ok [] Bot else Error [] env (TypeError ("Argument "++ show j ++" is not the negation of argument "++show i++"."))
ruleNotEilm env [b@(_, ArgForm (Not _)), a@(_, ArgForm _)] r = ruleNotEilm env [a, b] r;
ruleNotEilm env [(_, ArgForm _), (j, _)]               _ = Error [] env (RuleArgError j "Needs to be a not formula.")
ruleNotEilm env [(i, _), _]                       _ = Error [] env (RuleArgError i "Needs to be a formula.")
ruleNotEilm env forms                        _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 2 )

ruleBottomElim :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleBottomElim _ [(_, ArgForm Bot)] r = Ok [] r
ruleBottomElim env [_]           _ = Error [] env (RuleArgError 1 "Needs to be a bottom formula.")
ruleBottomElim env forms         _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleNotNotIntro :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleNotNotIntro _ [(_, ArgForm a)] _ = Ok [] (Not (Not a))
ruleNotNotIntro env [_]         _ = Error [] env (RuleArgError 1 "Needs to be a formula.")
ruleNotNotIntro env forms       _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleNotNotElim :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleNotNotElim _ [(_, ArgForm (Not (Not a)))] _ = Ok [] a
ruleNotNotElim env [_]                     _ = Error [] env (RuleArgError 1 "Needs to be a not not formula.")
ruleNotNotElim env forms                   _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleMT :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleMT env [(i, ArgForm (Impl a b)), (j, ArgForm (Not c))] _ = if b == c then Ok [] (Not a) else Error [] env (RuleConcError ("Conclusion in argument "++show i++" did not match argument "++ show j ++"."))
ruleMT env [b@(_, ArgForm (Not _)), a@(_, ArgForm (Impl _ _))] r = ruleMT env [a, b] r
ruleMT env [(_, ArgForm (Impl _ _)), (j, _)]               _ = Error [] env (RuleArgError j "Needs to be a not formula.")
ruleMT env [(i, _), _]                              _ = Error [] env (RuleArgError i "Needs to be an implication formula.")
ruleMT env forms                               _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

rulePBC :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
rulePBC _ [(_, ArgProof (Proof _ [Not a] Bot))] _ = Ok [] a
rulePBC env [(_, ArgProof (Proof _ [Not _] _))]   _ = Error [] env (RuleArgError 1 "Conclusion needs to be a bot formula.")
rulePBC env [(_, ArgProof (Proof _ [_] _))]       _ = Error [] env (RuleArgError 1 "Premise needs to be a not formula.")
rulePBC env [_]                            _ = Error [] env (RuleArgError 1 "Needs to be a proof.")
rulePBC env forms                          _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleLEM :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleLEM env [] r@(Or a (Not b)) = if a == b then Ok [] r else Error [] env (RuleConcError "Right hand side is not the negation of the left hand side.")
ruleLEM env []                _ = Error [] env (RuleConcError "The conclusion must be an or statement.")
ruleLEM env forms             _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 0 )

ruleEqI :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleEqI env [] r@(Eq a b) = if a == b then Ok [] r else Error [] env (RuleConcError "Left and right hand side are not the same.")
ruleEqI env [] _  = Error [] env (RuleConcError "The conclusion must be an eq statement.")
ruleEqI env forms _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 0 )

ruleEqE:: Env-> [(Integer, Arg)] -> Formula -> Result Formula
ruleEqE env _ _ = Error [] env (UnknownError "Unimplemented.")
--ruleEqE env forms _ = Error [] (RuleArgCountError env (toInteger $ List.length forms) 0 )

ruleAllE:: Env-> [(Integer, Arg)] -> Formula -> Result Formula
ruleAllE env [(_, ArgForm (All x a)), (_, ArgTerm t@(Term _ []))] _ = replaceFree env x t a
ruleAllE env [(_, ArgForm (All _ _)), (j, _)] _ = Error [] env (RuleArgError j  "Must be an free variable.")
ruleAllE env [(i, _), (_, _)] _ = Error [] env (RuleArgError i  "Must be an for all formula.")
ruleAllE env forms _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 2 )

ruleAllI:: Env-> [(Integer, Arg)] -> Formula -> Result Formula
ruleAllI env [(_, ArgProof (Proof [t] [] a))] (All x b) = case replaceFree env x t b of 
    Error warns env err -> Error warns env err --TODO specify argument error?
    Ok warns c -> if a==c then Ok warns (All x b) else Error [] env (RuleConcError "The given formula did not match the conclusion.")
ruleAllI env [(_, ArgProof (Proof [_] [] _))] _ = Error [] env (RuleConcError "The conclusion must be an for all formula.")
ruleAllI env [(i, _)] _ = Error [] env (RuleArgError i  "Must be an box.")
ruleAllI env forms _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleSomeE:: Env-> [(Integer, Arg)] -> Formula -> Result Formula
ruleSomeE env [(_,ArgForm (Some x a)), (_, ArgProof(Proof [t] [b] c))] _ = case replaceFree env t x a of
    Error warns env err -> Error warns env err --TODO specify argument error?
    Ok warns d -> if b == d then Ok [] c else Error warns env (RuleConcError "The given formula did not match the conclusion.")
ruleSomeE env [(_,ArgForm (Some _ _)), (j, _)] _ = Error [] env (RuleArgError j  "Must be an proof.")
ruleSomeE env [(_,_), (j, _)] _ = Error [] env (RuleArgError j  "Must be an formula some formula.")
ruleSomeE env forms _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 2 )

ruleSomeI:: Env-> [(Integer, Arg)] -> Formula -> Result Formula
ruleSomeI env [(_, ArgForm a),  (_, ArgTerm t@(Term _ []))] r@(Some x b) = case replaceFree env x t b of
    Error warns env err -> Error warns env err --TODO specify argument error?
    Ok warns c -> if a == c then Ok warns (Some x a) else Error [] env (RuleConcError "The given formula did not match the conclusion.")
ruleSomeI env [(_, ArgForm _),  (_, ArgTerm (Term _ []))] _ = Error [] env (RuleConcError "The conclusion must be an exist formula.")
ruleSomeI env [(_, ArgForm _), (j, _)] _ = Error [] env (RuleArgError j  "Must be an free variable.")
ruleSomeI env [(i, _), (_, _)] _ = Error [] env (RuleArgError i  "Must be an formula.")
ruleSomeI env forms _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 2 )

isFree:: Env -> Term -> Bool
isFree _ _  = False

replaceFree:: Env -> Term -> Term -> Formula -> Result Formula 
replaceFree env _ _ _ = Error [] env (UnknownError "replaceFree is unimplemented.")