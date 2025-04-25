{- |
Module      : Backend.Environment
Description : Environment handling for the proof checker
Copyright   : (c) Your Organization, 2023
License     : GPL-3

This module provides environment manipulation functions for the proof checker,
including rule implementations, variable binding, and formula manipulation.
-}

{-# LANGUAGE InstanceSigs #-}

module Backend.Environment (
    -- * Environment creation and manipulation
    Env,
    newEnv,
    push,
    showPos,
    addUDefRule,
    
    -- * Variables and term handling
    bindVar,
    regTerm,
    addFresh,
    
    -- * Premises and conclusions
    addPrem,
    getPrems,
    getFreshs,
    
    -- * Reference handling
    addRefs,
    getRefs,
    getRef,
    pushPos,
    popPos,
    
    -- * Rule application
    applyRule,
    
    -- * Formula manipulation
    replaceInFormula,
    replaceInTerms,
    replaceInTerm,

    -- * Types
    Term(..),
    Predicate(..),
    Formula(..),
    Proof(..),
    
    -- | Rule system
    Arg(..),
    UDefRule(..),
    
    -------------------------------------
    -- Error handling
    -------------------------------------
    -- | Result type
    Result(..),
    
    -- | Error types
    Error(..),
    ErrorKind(..),
    
    -- | Warning system
    Warning(..),
    WarningKind(..),
    Severity(..),
    
    -------------------------------------
    -- Environment
    -------------------------------------
    Env(..),
    Ref(..),
    IDType(..),
    
    -------------------------------------
    -- Type aliases
    -------------------------------------
    Terms,
    Refs,
    
    -- | Error creation
    createRuleArgError,
    createArgCountError,
    createRuleConcError,
    createTypeError,
    createReferenceError,
    createRuleNotFoundError,
    createUnknownError,
    createMismatchedFormulaError,
    createSyntaxError

) where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)
import Data.Char (toLower)

import Data.Map as Map (Map)
import Data.Maybe (listToMaybe)

----------------------------------------------------------------------
-- Environment Creation and Manipulation
----------------------------------------------------------------------

-- | Create a new empty environment with predefined rules
newEnv :: Env
newEnv = Env {
    prems = [],
    depth = 0,
    fresh = [],
    refs  = Map.empty,
    rules = Map.fromList [
        -- Propositional logic rules
        ("copy", ruleCopy), ("COPY", ruleCopy), ("C", ruleCopy),
        ("REITERATION", ruleCopy), ("R", ruleCopy),
        
        -- Conjunction rules
        ("AndI", ruleAndI), ("&I", ruleAndI), ("∧I", ruleAndI),
        ("AndEL", ruleAndEL), ("&EL", ruleAndEL), ("∧EL", ruleAndEL),
        ("AndER", ruleAndER), ("&ER", ruleAndER), ("∧ER", ruleAndER),
        
        -- Disjunction rules
        ("OrIL", ruleOrIL), ("|IL", ruleOrIL), ("∨IL", ruleOrIL),
        ("OrIR", ruleOrIR), ("|IR", ruleOrIR), ("∨IR", ruleOrIR),
        ("OrE", ruleOrE), ("|E", ruleOrE), ("∨E", ruleOrE),
        
        -- Implication rules
        ("ImplI", ruleImplI), ("IfI", ruleImplI), 
        ("->I", ruleImplI), ("→I", ruleImplI),
        ("ImplE", ruleImplE), ("IfE", ruleImplE), 
        ("->E", ruleImplE), ("→E", ruleImplE),
        
        -- Negation rules
        ("NotI", ruleNotI), ("!I", ruleNotI), ("¬I", ruleNotI),
        ("NotE", ruleNotE), ("!E", ruleNotE), ("¬E", ruleNotE),
        ("NotNotI", ruleNotNotI), ("!!I", ruleNotNotI), ("¬¬I", ruleNotNotI),
        ("NotNotE", ruleNotNotE), ("!!E", ruleNotNotE), ("¬¬E", ruleNotNotE),
        
        -- Bottom rules
        ("BotE", ruleBottomE), ("botE", ruleBottomE), 
        ("#E", ruleBottomE), ("⊥E", ruleBottomE),
        
        -- Meta rules
        ("MT", ruleMT), ("PBC", rulePBC), ("LEM", ruleLEM),
        
        -- Equality rules
        ("EqI", ruleEqI), ("=I", ruleEqI),
        ("EqE", ruleEqE), ("=E", ruleEqE),
        
        -- Quantifier rules
        ("AllE", ruleAllE), ("forallE", ruleAllE), ("∀E", ruleAllE),
        ("AllI", ruleAllI), ("forallI", ruleAllI), ("∀I", ruleAllI),
        ("SomeE", ruleSomeE), ("existsE", ruleSomeE), ("∃E", ruleSomeE),
        ("SomeI", ruleSomeI), ("existsI", ruleSomeI), ("∃I", ruleSomeI)
    ],
    user_rules = Map.empty,
    pos  = [],
    rule = "",
    bound = Map.empty,
    ids = Map.empty
}

-- | Add a user-defined rule to the environment
addUDefRule :: Env -> String -> UDefRule -> Env
addUDefRule env name r = env{user_rules = Map.insert name r (user_rules env)}

-- | Get a string representation of the current position in the proof
showPos :: Env -> String
showPos env = if rule env == "" then p else p ++ ":" ++ r ++ " "
  where 
    p = "[" ++ List.intercalate " " (reverse [show x | x <- pos env]) ++ "]"
    r = rule env

-- | Push a new context to the environment (used when entering a subproof or box)
-- This resets the list of premises for the new scope, keeping only assumptions.
push :: Env -> Env
push env = env { prems = [], fresh = [], depth = depth env + 1}

----------------------------------------------------------------------
-- Variable and Term Handling
----------------------------------------------------------------------

-- | Bind a variable in the current environment
bindVar :: Env -> Term -> Result Env
bindVar env (Term x []) = if Map.member x (bound env)
    then Err [] env (createUnknownError env ("Trying to rebind " ++ show x ++ "."))
    else Ok [] env { bound = Map.insert x () (bound env)}
bindVar env _ = Err [] env (createUnknownError env "Unable to bind a function.")

-- | Check if a term is a free variable in the current environment
isFreeVar :: Env -> Term -> Result Bool
isFreeVar env (Term x []) = if Map.notMember x (bound env) 
    then Ok [] True 
    else Ok [] False
isFreeVar env _ = Err [] env (createUnknownError env "A function cannot be a free variable.")

-- | Check if a term is a bound variable in the current environment
isBoundVar :: Env -> Term -> Result Bool
isBoundVar env (Term x []) = if Map.member x (bound env) 
    then Ok [] True 
    else Ok [] False
isBoundVar env _ = Err [] env (createUnknownError env "A function cannot be a bound variable.")

-- | Register a term in the environment
regTerm :: Env -> Term -> Result Env
regTerm env (Term x param) = case Map.lookup x (ids env) of
    Nothing -> Ok [] env {ids = Map.insert x (IDTypeTerm n) (ids env)}
    Just t -> case t of
         IDTypeTerm i -> if i == n 
            then Ok [] env 
            else Err [] env (createUnknownError env 
                  ("Trying to redefine " ++ show x ++ "."))
         IDTypePred _ -> Err [] env (createUnknownError env 
                          ("Trying to redefine " ++ show x ++ " as a term."))
  where n = toInteger (List.length param)

-- | Add a fresh variable to the environment
addFresh :: Env -> Term -> Result Env
addFresh env x@(Term _ []) = Ok [] env { fresh = fresh env ++ [x]}
addFresh env x = Err [] env (createUnknownError env 
                  ("Cannot add function " ++ show x ++ " as a fresh variable."))

----------------------------------------------------------------------
-- Premise Handling
----------------------------------------------------------------------

-- | Add a premise to the current environment
addPrem :: Env -> Formula -> Result Env
addPrem env prem = Ok [] (env { prems = prems env ++ [prem] })

-- | Get all premises from the current environment
getPrems :: Env -> [Formula]
getPrems = prems

-- | Get all fresh variables from the current environment
getFreshs :: Env -> [Term]
getFreshs = fresh

----------------------------------------------------------------------
-- Reference Handling
----------------------------------------------------------------------

-- | Add references to the environment
-- References map labels to arguments (e.g., proofs, formulas, or terms).
addRefs :: Env -> [Ref] -> Arg -> Env
addRefs env labels form = env { 
    refs = Map.union (refs env) (Map.fromList [(label, (0, form)) | label <- labels])
}

-- | Get all references matching the provided refs
getRefs :: Env -> [Ref] -> Result (Env, [Arg])
getRefs env [] = Ok [] (env, [])
getRefs env (x : xs) =
    case getRefs env xs of
        Err warns env_e err -> Err warns env_e err
        Ok warns1 (env1, args) ->
            case getRef env1 x of
                Err warns env_e err -> Err (warns ++ warns1) env_e err
                Ok warns2 (env2, arg) -> Ok (warns1 ++ warns2) (env2, arg : args)

-- | Get a single reference
-- Increments the reference count when accessed
getRef :: Env -> Ref -> Result (Env, Arg)
getRef env ref = case Map.lookup ref (refs env) of
    Nothing -> Err [] env (createReferenceError env ref 
              ("Reference " ++ show ref ++ " was not found."))
    Just (count, arg) -> Ok [] (env { 
        refs = Map.insert ref (count + 1, arg) (refs env) 
    }, arg)

-- | Push a list of references onto the position stack
pushPos :: Env -> [Ref] -> Env
pushPos env r = env {pos = r ++ pos env}

-- | Remove n references from the position stack
popPos :: Env -> Integer -> Env
popPos env n = env {pos = drop (fromIntegral n) (pos env)}

----------------------------------------------------------------------
-- Rule Application
----------------------------------------------------------------------

-- | Apply a rule to a list of arguments and check the result
applyRule :: Env -> String -> [Arg] -> Formula -> Result Formula
applyRule e name args res =
    let env = e{rule=name}
        availableRules = Map.keys (rules env) ++ Map.keys (user_rules env)
    in case Map.lookup name (rules env) of
        Just rule_f ->
            case rule_f env (zip [1..] args) res of
                Err warns env_e err -> Err warns env_e err
                Ok warns res_t ->
                    if cmp env res_t res then Ok warns res_t
                    else Err warns env (createRuleConcError env 
                         ("Wrong conclusion when using rule, expected " ++ 
                          show res ++ ", got " ++ show res_t))
        Nothing -> case Map.lookup name (user_rules env) of
            Just rule_s -> case applyUDefRule env rule_s (zip [1..] args) of 
                Err warns env_e err -> Err warns env_e err
                Ok warns res_t ->
                    if cmp env res_t res then Ok warns res_t
                    else Err warns env (createRuleConcError env 
                         ("Wrong conclusion when using rule, expected " ++ 
                          show res ++ ", got " ++ show res_t))
            Nothing -> 
                if null name then
                    Err [] env (createNoRuleProvidedError env "No rule was provided.")
                else
                let
                    prefixMatches = filter (List.isPrefixOf name) availableRules
                    
                    caseInsensitiveMatches = 
                        if null prefixMatches then
                            filter (\r -> map toLower name `List.isPrefixOf` map toLower r) availableRules
                        else []
                    
                    substringMatches = 
                        if null prefixMatches && null caseInsensitiveMatches then
                            filter (\r -> name `List.isInfixOf` r || 
                                         map toLower name `List.isInfixOf` map toLower r) availableRules
                        else []
                    
                    allSuggestions = take 5 (prefixMatches ++ caseInsensitiveMatches ++ substringMatches)
                    
                    baseError = createRuleNotFoundError env name
                    updatedError = 
                        if null allSuggestions 
                        then baseError
                        else baseError { 
                            errSuggestions = errSuggestions baseError ++ 
                                ["Did you mean: " ++ 
                                List.intercalate ", " allSuggestions ++ "?"]
                        }
                in Err [] env updatedError

-- | Apply a user-defined rule
applyUDefRule :: Env -> UDefRule -> [(Integer, Arg)] -> Result Formula
applyUDefRule env (UDefRule ins out) args = 
    if (toInteger $ List.length ins) /= (toInteger $ List.length args) 
        then Err [] env (createArgCountError env 
                        (toInteger $ List.length args) 
                        (toInteger $ List.length ins))
        else do
            ph <- findPlaceholdersInArgs env ins args Map.empty
            replacePlaceholders env out ph

-- | Find placeholders in arguments for user-defined rules
findPlaceholdersInArgs :: Env -> [Formula] -> [(Integer, Arg)] -> 
                         Map.Map Predicate Formula -> 
                         Result (Map.Map Predicate Formula)
findPlaceholdersInArgs env [] [] ph = Ok [] ph
findPlaceholdersInArgs env [a] [(i, b)] ph = case b of
    ArgForm b_f -> findPlaceholdersInArg env i a b_f ph
    _ -> Err [] env (createRuleArgError env i 
          "Arguments in user defined rules must be formulas.")
findPlaceholdersInArgs env (a:as) ((i, b):bs) ph = case b of
    ArgForm b_f -> do
        ph1 <- findPlaceholdersInArg env i a b_f ph
        ph2 <- findPlaceholdersInArgs env as bs ph1
        Ok [] ph2
    _ -> Err [] env (createRuleArgError env i 
          "Arguments in user defined rules must be formulas.")
findPlaceholdersInArgs env _ _ _ = Err [] env (createUnknownError env 
                                   "Mismatch in number of args and signature.")

-- | Match placeholders in a single argument
findPlaceholdersInArg :: Env -> Integer -> Formula -> Formula -> 
                        Map.Map Predicate Formula -> 
                        Result (Map.Map Predicate Formula)
findPlaceholdersInArg env i (And l1 r1) (And l2 r2) ph = do
    phl <- findPlaceholdersInArg env i l1 l2 ph
    phr <- findPlaceholdersInArg env i r1 r2 phl
    Ok [] phr
findPlaceholdersInArg env i (Or l1 r1) (Or l2 r2) ph = do
    phl <- findPlaceholdersInArg env i l1 l2 ph
    phr <- findPlaceholdersInArg env i r1 r2 phl
    Ok [] phr
findPlaceholdersInArg env i (Impl l1 r1) (Impl l2 r2) ph = do
    phl <- findPlaceholdersInArg env i l1 l2 ph
    phr <- findPlaceholdersInArg env i r1 r2 phl
    Ok [] phr
findPlaceholdersInArg _ _ Bot Bot ph = Ok [] ph
findPlaceholdersInArg env i (Not a) (Not b) ph = findPlaceholdersInArg env i a b ph
findPlaceholdersInArg env i (Pred a) b ph = case Map.lookup a ph of
    Nothing -> Ok [] (Map.insert a b ph)
    Just c -> if b == c
        then Ok [] ph
        else Err [] env (createRuleArgError env i 
              "Did not match previous instance of placeholder.")
findPlaceholdersInArg env i (All x a) (All y b) ph = if x == y 
    then findPlaceholdersInArg env i a b ph
    else do
        phi <- replaceInFormula env y x b
        findPlaceholdersInArg env i a phi ph
findPlaceholdersInArg env i (Some x a) (Some y b) ph = if x == y 
    then findPlaceholdersInArg env i a b ph
    else do
        phi <- replaceInFormula env y x b
        findPlaceholdersInArg env i a phi ph
findPlaceholdersInArg env i _ _ ph = Err [] env (createUnknownError env "Unimplemented.")

-- | Replace placeholders in a formula
replacePlaceholders :: Env -> Formula -> Map.Map Predicate Formula -> Result Formula
replacePlaceholders env (And l r) ph = do
    res_l <- replacePlaceholders env l ph
    res_r <- replacePlaceholders env r ph
    Ok [] (And res_l res_r)
replacePlaceholders env (Or l r) ph = do
    res_l <- replacePlaceholders env l ph
    res_r <- replacePlaceholders env r ph
    Ok [] (Or res_l res_r)
replacePlaceholders env (Impl l r) ph = do
    res_l <- replacePlaceholders env l ph
    res_r <- replacePlaceholders env r ph
    Ok [] (Impl res_l res_r)
replacePlaceholders _ Bot _ = Ok [] Bot
replacePlaceholders env (Not a) ph = do
    res <- replacePlaceholders env a ph
    Ok [] (Not res)
replacePlaceholders env (Pred p) ph = case Map.lookup p ph of
    Nothing -> Err [] env (createUnknownError env "Unimplemented.")
    Just res -> Ok [] res
replacePlaceholders env _ _ = Err [] env (createUnknownError env "Unimplemented.")

----------------------------------------------------------------------
-- Formula manipulation
----------------------------------------------------------------------

-- | Check if a term can be freely substituted for a variable in a formula
isFreeFor :: Env -> Term -> Term -> Formula -> Result Bool
isFreeFor _ _ _ Bot = Ok [] True
isFreeFor _ _ _ (Pred _) = Ok [] True
isFreeFor _ _ _ (Eq _ _) = Ok [] True
isFreeFor env t x (And l r) = do
    res_l <- isFreeFor env t x l
    res_r <- isFreeFor env t x r
    Ok [] (res_l && res_r)
isFreeFor env t x (Or l r) = do
    res_l <- isFreeFor env t x l
    res_r <- isFreeFor env t x r
    Ok [] (res_l && res_r)
isFreeFor env t x (Impl l r) = do
    res_l <- isFreeFor env t x l
    res_r <- isFreeFor env t x r
    Ok [] (res_l && res_r)
isFreeFor env t x (All y phi) = if Set.member x (freeVarForm (All y phi))
    then case isFreeFor env t x phi of
        Err warns env_e err -> Err warns env_e err
        Ok warns a -> if Set.notMember y (freeVarTerm t) && a
        then Ok warns True
        else Ok warns False
    else Ok [] True
isFreeFor env t x (Some y f) = case isFreeFor env t x f of
    Err warns env_e err -> Err warns env_e err
    Ok warns False -> Ok warns False
    Ok warns True -> Ok warns (Set.member x (freeVarForm (Some y f)) && 
                               Set.notMember y (freeVarTerm t))
isFreeFor env t x (Not f) = isFreeFor env t x f
isFreeFor env _ _ Nil = Err [] env (createUnknownError env "Nil formula.")

-- | Get the free variables in a term
freeVarTerm :: Term -> Set.Set Term
freeVarTerm (Term s []) = Set.singleton (Term s [])
freeVarTerm (Term _ [term]) = Set.singleton term
freeVarTerm (Term _ terms) = Set.unions [freeVarTerm term | term <- terms]

-- | Get the free variables in a formula
freeVarForm :: Formula -> Set.Set Term
freeVarForm Bot = Set.empty
freeVarForm (Pred (Predicate _ terms)) = Set.unions [freeVarTerm term | term <- terms]
freeVarForm (Eq l r) = Set.union (freeVarTerm l) (freeVarTerm r)
freeVarForm (And l r) = Set.union (freeVarForm l) (freeVarForm r)
freeVarForm (Or l r) = Set.union (freeVarForm l) (freeVarForm r)
freeVarForm (Impl l r) = Set.union (freeVarForm l) (freeVarForm r)
freeVarForm (All x f) = Set.difference (freeVarForm f) (Set.singleton x)
freeVarForm (Some x f) = Set.difference (freeVarForm f) (Set.singleton x)
freeVarForm (Not f) = freeVarForm f
freeVarForm Nil = Set.empty

-- | Replace a term in another term
replaceInTerm :: Env -> Term -> Term -> Term -> Result Term
replaceInTerm env var@(Term x []) t a@(Term y []) = case isFreeVar env var of
    Err warns env_e err -> Err warns env_e err
    Ok warns is_free -> if is_free && (x == y) 
        then Ok warns t 
        else Ok warns a
replaceInTerm env x@(Term _ []) t (Term f args) = case replaceInTerms env x t args of
    Err warns env_e err -> Err warns env_e err
    Ok warns new_args -> Ok warns (Term f new_args)
replaceInTerm env _ _ _ = Err [] env (createUnknownError env "Needs to be a variable.")

-- | Replace each free occurrence of a variable with a term in a list of terms
replaceInTerms :: Env -> Term -> Term -> [Term] -> Result [Term]
replaceInTerms _ (Term _ []) _ [] = Ok [] []
replaceInTerms env x@(Term _ []) t (arg : args) = do
    args_new <- replaceInTerms env x t args
    arg_new <- replaceInTerm env x t arg
    Ok [] (arg_new : args_new)
replaceInTerms env _ _ _ = Err [] env (createUnknownError env "Needs to be a variable.")

-- | Replace each free occurrence of a variable with a term in a formula
replaceInFormula :: Env -> Term -> Term -> Formula -> Result Formula
replaceInFormula _ (Term _ []) _ Bot = Ok [] Bot
replaceInFormula _ (Term _ []) _ p@(Pred (Predicate _ [])) = Ok [] p
replaceInFormula env x@(Term _ []) t (Pred (Predicate p args)) = do
    args_new <- replaceInTerms env x t args
    Ok [] (Pred (Predicate p args_new))
replaceInFormula env x@(Term _ []) t (Eq l r) = do
    l_new <- replaceInTerm env x t l
    r_new <- replaceInTerm env x t r
    Ok [] (Eq l_new r_new)
replaceInFormula env x@(Term _ []) t (And l r) = do
    l_new <- replaceInFormula env x t l
    r_new <- replaceInFormula env x t r
    Ok [] (And l_new r_new)
replaceInFormula env x@(Term _ []) t (Or l r) = do
    l_new <- replaceInFormula env x t l
    r_new <- replaceInFormula env x t r
    Ok [] (Or l_new r_new)
replaceInFormula env x@(Term _ []) t (Impl l r) = do
    l_new <- replaceInFormula env x t l
    r_new <- replaceInFormula env x t r
    Ok [] (Impl l_new r_new)
replaceInFormula env x@(Term _ []) t (All y a) = 
    if x == y then Ok [] (All y a)
    else case isFreeFor env t x (All y a) of
        Err warns env_e err -> Err warns env_e err
        Ok warns_free False -> Err warns_free env 
            (createUnknownError env "Cannot replace variable")
        Ok warns_free True -> do
            new_env <- bindVar env y
            y_new <- replaceInTerm new_env x t y
            a_new <- replaceInFormula new_env x t a
            Ok warns_free (All y_new a_new)
replaceInFormula env x@(Term _ []) t (Some y a) =
    if x == y then Ok [] (Some y a)
    else case isFreeFor env t x (Some y a) of
        Err warns env_e err -> Err warns env_e err
        Ok warns_free False -> Err warns_free env 
            (createUnknownError env "Cannot replace variable")
        Ok warns_free True -> do
            new_env <- bindVar env y
            y_new <- replaceInTerm new_env x t y
            a_new <- replaceInFormula new_env x t a
            Ok warns_free (Some y_new a_new)
replaceInFormula env x@(Term _ []) t (Not a) = do
    a_new <- replaceInFormula env x t a
    Ok [] (Not a_new)
replaceInFormula env (Term _ []) _ Nil = Err [] env 
    (createUnknownError env "Trying to do a replace on nil formula.")
replaceInFormula env x _ _ = Err [] env 
    (createUnknownError env (show x ++ " needs to be a variable."))

-- | Compare two formulas with environment
cmp :: Env -> Formula -> Formula -> Bool
cmp _ (Pred a) (Pred b) = a == b
cmp env (And a1 a2) (And b1 b2) = cmp env a1 b1 && cmp env a2 b2
cmp env (Or a1 a2) (Or b1 b2) = cmp env a1 b1 && cmp env a2 b2
cmp env (Impl a1 a2) (Impl b1 b2) = cmp env a1 b1 && cmp env a2 b2
cmp env (Eq a1 a2) (Eq b1 b2) = a1 == b1 && a2 == b2

cmp env (All x a) (All y b) =
    (x == y && cmp env a b) ||
    (case isFreeFor env y x b of
        Ok _ True -> case replaceInFormula env x y b of
            Ok _ replaced -> cmp env a replaced
            _ -> False
        _ -> False)

cmp env (Some x a) (Some y b) =
    (x == y && cmp env a b) ||
    (case isFreeFor env y x b of
        Ok _ True -> case replaceInFormula env x y b of
            Ok _ replaced -> cmp env a replaced
            _ -> False
        _ -> False)

cmp env (Not a) (Not b) = cmp env a b
cmp _ Bot Bot = True
cmp _ Nil Nil = True
cmp _ _ _ = False
----------------------------------------------------------------------
-- Predefined Rules
----------------------------------------------------------------------

-- | Copy rule (identity)
ruleCopy :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleCopy _ [(_, ArgForm form)] _ = Ok [] form
ruleCopy env forms _ = Err [] env (createArgCountError env 
                       (toInteger $ List.length forms) 1)

-- | Introduction of conjunction
ruleAndI :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleAndI _ [(_, ArgForm a), (_, ArgForm b)] _ = Ok [] (And a b)
ruleAndI env [_, (j, ArgForm _)] _ = Err [] env (createRuleArgError env j 
                                     "Needs to be a formula.")
ruleAndI env [(i, _), _] _ = Err [] env (createRuleArgError env i 
                             "Needs to be a formula.")
ruleAndI env forms _ = Err [] env (createArgCountError env 
                        (toInteger $ List.length forms) 2)

-- | Elimination of conjunction (left)
ruleAndEL :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleAndEL _ [(_, ArgForm (And l _))] _ = Ok [] l
ruleAndEL env [_] _ = Err [] env (createRuleArgError env 1 
                      "Needs to be an and formula.")
ruleAndEL env forms _ = Err [] env (createArgCountError env 
                        (toInteger $ List.length forms) 1)

-- | Elimination of conjunction (right)
ruleAndER :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleAndER _ [(_, ArgForm (And _ r))] _ = Ok [] r
ruleAndER env [_] _ = Err [] env (createRuleArgError env 1 
                      "Needs to be an and formula.")
ruleAndER env forms _ = Err [] env (createArgCountError env 
                        (toInteger $ List.length forms) 1)

-- | Introduction of disjunction (left)
ruleOrIL :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleOrIL env [(_, ArgForm a)] r@(Or b _) = 
    if cmp env a b then Ok [] r
    else Err [] env (createRuleArgError env 1 
         "Did not match left hand side of conclusion.")
ruleOrIL env [(_, ArgForm _)] _ = Err [] env (createRuleConcError env 
                                  "Conclusion needs to be an or formula.")
ruleOrIL env [_] (Or _ _) = Err [] env (createRuleArgError env 1 
                            "Needs to be a formula.")
ruleOrIL env forms _ = Err [] env (createArgCountError env 
                        (toInteger $ List.length forms) 1)

-- | Introduction of disjunction (right)
ruleOrIR :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleOrIR env [(_, ArgForm a)] r@(Or _ b) = 
    if cmp env a b then Ok [] r
    else Err [] env (createRuleArgError env 1 
         "Did not match right hand side of conclusion.")
ruleOrIR env [(_, ArgForm _)] _ = Err [] env (createRuleConcError env 
                                  "Conclusion needs to be an or formula.")
ruleOrIR env [_] (Or _ _) = Err [] env (createRuleArgError env 1 
                            "Needs to be a formula.")
ruleOrIR env forms _ = Err [] env (createArgCountError env 
                        (toInteger $ List.length forms) 1)

-- | Elimination of disjunction
ruleOrE :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleOrE env [(_, ArgForm (Or a b)), (j, ArgProof (Proof _ [p1] c1)), 
              (k, ArgProof (Proof _ [p2] c2))] _ =
    if cmp env a p1 then
        if cmp env b p2 then
            if cmp env c1 c2 then Ok [] c1 
            else Err [] env (createRuleConcError env 
                 "The conclusions of the two proofs did not match.")
        else Err [] env (createRuleArgError env k 
             "The premise of the proof did not match the right hand side of the or statement.")
    else Err [] env (createRuleArgError env j 
         "The premise of the proof did not match the left hand side of the or statement.")
ruleOrE env [b@(_, ArgProof _), a@(_, ArgForm _), c@(_, ArgProof _)] r = 
    ruleOrE env [a, b, c] r
ruleOrE env [b@(_, ArgProof _), c@(_, ArgProof _), a@(_, ArgForm _)] r = 
    ruleOrE env [a, b, c] r
ruleOrE env [(_, ArgForm (Or _ _)), (_, ArgProof _), (k, _)] _ = 
    Err [] env (createRuleArgError env k "Needs to be a proof.")
ruleOrE env [(_, ArgForm (Or _ _)), (j, _), _] _ = 
    Err [] env (createRuleArgError env j "Needs to be a proof.")
ruleOrE env [(i, _), _, _] _ = 
    Err [] env (createRuleArgError env i "Needs to be an or formula.")
ruleOrE env forms _ = 
    Err [] env (createArgCountError env (toInteger $ List.length forms) 3)

-- | Introduction of implication
ruleImplI :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleImplI _ [(_, ArgProof (Proof _ [a] b))] _ = Ok [] (Impl a b)
ruleImplI env [_] _ = Err [] env (createRuleArgError env 1 
                      "Needs to be a proof with a single premise")
ruleImplI env forms _ = Err [] env (createArgCountError env 
                        (toInteger $ length forms) 1)

-- | Elimination of implication (modus ponens)
ruleImplE :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleImplE env [(i, ArgForm x@(Impl a b)), (j, ArgForm y@(Impl c d))] _ 
    | cmp env x c = Ok [] d 
    | cmp env y a = Ok [] b
    | otherwise = Err [] env (createRuleArgError env j ("Premise did not match argument "++ show i ++"."))
ruleImplE env [(i, ArgForm a), (j, ArgForm (Impl b c))] _ =
    if cmp env a b then Ok [] c
    else Err [] env (createRuleArgError env j 
         ("Premise did not match argument "++ show i ++"."))
ruleImplE env [b@(_, ArgForm (Impl _ _)), a@(_, ArgForm _)] r = 
    ruleImplE env [a,b] r
ruleImplE env [(_, ArgForm _), (j, _)] _ = 
    Err [] env (createRuleArgError env j "Needs to be an implication formula")
ruleImplE env [(i, _), _] _ = 
    Err [] env (createRuleArgError env i "Needs to be a formula")
ruleImplE env forms _ = 
    Err [] env (createArgCountError env (toInteger $ List.length forms) 2)

-- | Introduction of negation
ruleNotI :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleNotI _ [(_, ArgProof (Proof _ [a] Bot))] _ = Ok [] (Not a)
ruleNotI env [_] _ = Err [] env (createRuleArgError env 1 
                     "Needs to be a proof with the conclusion of bot")
ruleNotI env forms _ = Err [] env (createArgCountError env 
                        (toInteger $ List.length forms) 1)

-- | Elimination of negation
ruleNotE :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleNotE env [(i, ArgForm (Not a)), (j, ArgForm (Not b))] _
    | cmp env a (Not b) = Ok [] Bot 
    | cmp env (Not a) b = Ok [] Bot 
    | otherwise = Err [] env (createTypeError env ("Argument " ++ show j ++ " is not the negation of argument " ++ show i ++ "."))
ruleNotE env [(i, ArgForm a), (j, ArgForm (Not b))] _ = 
    if cmp env a b then Ok [] Bot 
    else Err [] env (createTypeError env 
         ("Argument " ++ show j ++ " is not the negation of argument " ++ show i ++ "."))
ruleNotE env [b@(_, ArgForm (Not _)), a@(_, ArgForm _)] r = 
    ruleNotE env [a, b] r
ruleNotE env [(_, ArgForm _), (j, _)] _ = 
    Err [] env (createRuleArgError env j "Needs to be a not formula.")
ruleNotE env [(i, _), _] _ = 
    Err [] env (createRuleArgError env i "Needs to be a formula.")
ruleNotE env forms _ = 
    Err [] env (createArgCountError env (toInteger $ List.length forms) 2)

-- | Elimination of bottom (ex falso quodlibet)
ruleBottomE :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleBottomE _ [(_, ArgForm Bot)] r = Ok [] r
ruleBottomE env [_] _ = Err [] env (createRuleArgError env 1 
                        "Needs to be a bottom formula.")
ruleBottomE env forms _ = Err [] env (createArgCountError env 
                          (toInteger $ List.length forms) 1)

-- | Introduction of double negation
ruleNotNotI :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleNotNotI _ [(_, ArgForm a)] _ = Ok [] (Not (Not a))
ruleNotNotI env [_] _ = Err [] env (createRuleArgError env 1 
                        "Needs to be a formula.")
ruleNotNotI env forms _ = Err [] env (createArgCountError env 
                          (toInteger $ List.length forms) 1)

-- | Elimination of double negation
ruleNotNotE :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleNotNotE _ [(_, ArgForm (Not (Not a)))] _ = Ok [] a
ruleNotNotE env [_] _ = Err [] env (createRuleArgError env 1 
                        "Needs to be a not not formula.")
ruleNotNotE env forms _ = Err [] env (createArgCountError env 
                          (toInteger $ List.length forms) 1)

-- | Modus tollens
ruleMT :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleMT env [(i, ArgForm (Impl a b)), (j, ArgForm (Not c))] _ = 
    if cmp env b c then Ok [] (Not a) 
    else Err [] env (createRuleConcError env 
         ("Conclusion in argument " ++ show i ++ 
          " did not match argument " ++ show j ++ "."))
ruleMT env [b@(_, ArgForm (Not _)), a@(_, ArgForm (Impl _ _))] r = 
    ruleMT env [a, b] r
ruleMT env [(_, ArgForm (Impl _ _)), (j, _)] _ = 
    Err [] env (createRuleArgError env j "Needs to be a not formula.")
ruleMT env [(i, _), _] _ = 
    Err [] env (createRuleArgError env i "Needs to be an implication formula.")
ruleMT env forms _ = 
    Err [] env (createArgCountError env (toInteger $ List.length forms) 2)

-- | Proof by contradiction
rulePBC :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
rulePBC _ [(_, ArgProof (Proof _ [Not a] Bot))] _ = Ok [] a
rulePBC env [(_, ArgProof (Proof _ [Not _] _))] _ = 
    Err [] env (createRuleArgError env 1 "Conclusion needs to be a bot formula.")
rulePBC env [(_, ArgProof (Proof [_] _ _))] _ = 
    Err [] env (createRuleArgError env 1 "Premise needs to be a not formula.")
rulePBC env [_] _ = Err [] env (createRuleArgError env 1 "Needs to be a proof.")
rulePBC env forms _ = Err [] env (createArgCountError env 
                      (toInteger $ List.length forms) 1)

-- | Law of excluded middle
ruleLEM :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleLEM env [] r@(Or a (Not b)) = 
    if cmp env a b then Ok [] r 
    else Err [] env (createRuleConcError env 
         "Right hand side is not the negation of the left hand side.")
ruleLEM env [] _ = Err [] env (createRuleConcError env 
                   "The conclusion must be an or statement.")
ruleLEM env forms _ = Err [] env (createArgCountError env 
                      (toInteger $ List.length forms) 0)

-- | Introduction of equality (reflexivity)
ruleEqI :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleEqI env [] r@(Eq a b) = 
    if a == b then Ok [] r 
    else Err [] env (createRuleConcError env 
         "Left and right hand side are not the same.")
ruleEqI env [] _ = Err [] env (createRuleConcError env 
                   "The conclusion must be an eq statement.")
ruleEqI env forms _ = Err [] env (createArgCountError env 
                      (toInteger $ List.length forms) 0)

-- | Elimination of equality (substitution)
ruleEqE :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleEqE env [(i, ArgForm (Eq t1 t2)), (j, ArgForm a), (_, ArgFormWith u phi)] _ = 
    case isFreeFor env t1 u phi of
        Err warns env_e err -> Err warns env_e err
        Ok warns_t1 False -> Err warns_t1 env (createRuleArgError env i 
                             (show t1 ++ " needs to be free for " ++ 
                              show u ++ " in " ++ show phi))
        Ok warns_t1 True -> case isFreeFor env t2 u phi of
            Err warns env_e err -> Err warns env_e err
            Ok warns_t2 False -> Err (warns_t1 ++ warns_t2) env 
                               (createRuleArgError env i (show t2 ++ 
                                " needs to be free for " ++ show u ++ 
                                " in " ++ show phi))
            Ok warns_t2 True -> case replaceInFormula env u t1 phi of
                Err warns env_e err -> Err (warns ++ warns_t1 ++ warns_t2) env_e err
                Ok warns1 b -> if not (cmp env a b)
                    then Err (warns_t1 ++ warns_t2 ++ warns1) env 
                         (createRuleArgError env j ("Do not match " ++ 
                          show phi ++ "[" ++ show t1 ++ "/" ++ show u ++ "]."))
                    else case replaceInFormula env u t2 phi of
                        Err warns env_e err -> 
                            Err (warns ++ warns_t1 ++ warns_t2 ++ warns1) env_e err
                        Ok warns2 c -> 
                            Ok (warns_t1 ++ warns_t2 ++ warns1 ++ warns2) c
ruleEqE env [(_, ArgForm (Eq _ _)), (_, ArgForm _), (k, _)] _ = 
    Err [] env (createRuleArgError env k "Must be a formula.")
ruleEqE env [(_, ArgForm (Eq _ _)), (j, _), (_, _)] _ = 
    Err [] env (createRuleArgError env j "Must be a formula.")
ruleEqE env [(i, _), (_, _), (_, _)] _ = 
    Err [] env (createRuleArgError env i "Must be an equality.")
ruleEqE env forms _ = 
    Err [] env (createArgCountError env (toInteger $ List.length forms) 3)

-- | Elimination of universal quantifier
ruleAllE :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleAllE env [(_, ArgForm (All x phi)), (j, ArgTerm t)] _ = 
    case isFreeFor env t x phi of
        Err warns env_e err -> Err warns env_e err
        Ok warns False -> Err warns env (createRuleArgError env j 
                         (show t ++ " needs to be free for " ++ 
                          show x ++ " in " ++ show phi))
        Ok warns1 True -> case replaceInFormula env x t phi of
            Err warns env_e err -> Err (warns ++ warns1) env_e err
            Ok warns2 res -> Ok (warns1 ++ warns2) res
ruleAllE env [(_, ArgForm (All _ _)), (j, _)] _ = 
    Err [] env (createRuleArgError env j "Must be a free variable.")
ruleAllE env [(i, _), (_, _)] _ = 
    Err [] env (createRuleArgError env i "Must be a for all formula.")
ruleAllE env forms _ = 
    Err [] env (createArgCountError env (toInteger $ List.length forms) 2)

-- | Introduction of universal quantifier
ruleAllI :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleAllI env [(_, ArgProof (Proof [t] [] a))] (All x b) = 
    case replaceInFormula env x t b of
        Err warns env_e err -> Err warns env_e err
        Ok warns c -> if cmp env a c then Ok warns (All x b) 
                     else Err [] env (createRuleConcError env 
                          "The given formula did not match the conclusion.")
ruleAllI env [(_, ArgProof (Proof [_] [] _))] _ = 
    Err [] env (createRuleConcError env "The conclusion must be a for all formula.")
ruleAllI env [(i, _)] _ = 
    Err [] env (createRuleArgError env i "Must be a box.")
ruleAllI env forms _ = 
    Err [] env (createArgCountError env (toInteger $ List.length forms) 1)

-- | Elimination of existential quantifier
ruleSomeE :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleSomeE env [(_, ArgForm (Some x a)), (_, ArgProof (Proof [x_0] [b] c))] _ = 
    case replaceInFormula env x x_0 a of
        Err warns env_e err -> Err warns env_e err
        Ok warns d -> if cmp env b d then Ok [] c 
                     else Err warns env (createRuleConcError env 
                          (show a ++ "[" ++ show x_0 ++ "/" ++ show x ++ 
                           "] resulted in " ++ show d ++ " and not " ++ 
                           show b ++ " as expected."))
ruleSomeE env [(_, ArgForm (Some _ _)), (j, b)] _ = 
    Err [] env (createRuleArgError env j ("Must be a proof not " ++ show b ++ "."))
ruleSomeE env [(i, a), (_, _)] _ = 
    Err [] env (createRuleArgError env i 
         ("Must be an Exists formula not " ++ show a ++ "."))
ruleSomeE env forms _ = 
    Err [] env (createArgCountError env (toInteger $ List.length forms) 2)

-- | Introduction of existential quantifier
ruleSomeI :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleSomeI env [(_, ArgForm a), (j, ArgTerm t@(Term _ _))] (Some x phi) = 
    case isFreeFor env t x phi of
        Err warns env_e err -> Err warns env_e err
        Ok warns False -> Err warns env (createRuleArgError env j 
                         (show t ++ " needs to be free for " ++ 
                          show x ++ " in " ++ show phi))
        Ok warns1 True -> case replaceInFormula env x t phi of
            Err warns env_e err -> Err warns env_e err
            Ok warns2 c -> if cmp env a c then Ok (warns1 ++ warns2) (Some x phi) 
                          else Err [] env (createRuleConcError env 
                              (show phi ++ "[" ++ show x ++ "/" ++ 
                               show t ++ "] resulted in " ++ show c ++ 
                               " and not " ++ show a ++ " as expected."))
ruleSomeI env [(_, ArgForm _), (_, ArgTerm (Term _ []))] r = 
    Err [] env (createRuleConcError env 
         ("The conclusion must be an exist formula not " ++ show r ++ "."))
ruleSomeI env [(_, ArgForm _), (j, _)] _ = 
    Err [] env (createRuleArgError env j "Must be a term.")
ruleSomeI env [(i, _), (_, _)] _ = 
    Err [] env (createRuleArgError env i "Must be a formula.")
ruleSomeI env forms _ = 
    Err [] env (createArgCountError env (toInteger $ List.length forms) 2)


----------------------------------------------------------------------
-- Type aliases
----------------------------------------------------------------------

-- | A list of terms
type Terms = [Term]

-- | A list of references
type Refs = [Ref]

-- | A list of formulas
type Formulas = [Formula]

-- | A tuple of argument index and argument
type ArgTup = (Integer, Arg)

----------------------------------------------------------------------
-- Core data types for logic
----------------------------------------------------------------------

-- | A term, either a variable or function application with arguments
data Term = Term String [Term] deriving Ord

-- | A predicate with a name and list of term arguments
data Predicate = Predicate String [Term] deriving Ord

-- | A logical formula in first-order logic
data Formula =
    Pred Predicate              -- ^ Predicate application
  | And Formula Formula         -- ^ Logical conjunction
  | Or Formula Formula          -- ^ Logical disjunction
  | Impl Formula Formula        -- ^ Logical implication
  | Eq Term Term                -- ^ Equality between terms
  | All Term Formula            -- ^ Universal quantification
  | Some Term Formula           -- ^ Existential quantification
  | Not Formula                 -- ^ Logical negation
  | Bot                         -- ^ Bottom (false)
  | Nil                         -- ^ Nil (used for incomplete formulas)

-- | A proof consisting of fresh variables, premises, and a conclusion
data Proof = Proof [Term] [Formula] Formula

-- | An argument to a rule application
data Arg = 
    ArgProof Proof                -- ^ A proof box as an argument 
  | ArgForm Formula               -- ^ A formula as an argument
  | ArgTerm Term                  -- ^ A term as an argument
  | ArgFormWith Term Formula      -- ^ A formula with a term variable (for substitution)

-- | A user-defined rule with input formulas and output formula
data UDefRule = UDefRule [Formula] Formula

----------------------------------------------------------------------
-- Error and Result system
----------------------------------------------------------------------

-- | Severity levels for warnings
data Severity = Hint | Low | Medium | High deriving (Eq, Ord, Show)

-- | Types of warnings that can be generated
data WarningKind =
    UnusedReference Ref                -- ^ A reference was declared but never used
  | IncompleteProof Formula Formula    -- ^ A proof is incomplete (expected vs actual)
  | PossibleSimplification String      -- ^ A proof step could be simplified
  | RedundantStep                      -- ^ A step in the proof is redundant
  | StyleIssue String                  -- ^ A style issue with the proof
  | GeneralWarning String              -- ^ A general warning
  deriving (Show)

-- | A warning with location, severity, and message information
data Warning = Warning {
  warnLocation :: Maybe Ref,        -- ^ Optional location of the warning
  warnSeverity :: Severity,         -- ^ Severity level of the warning
  warnKind :: WarningKind,          -- ^ Type of warning
  warnMessage :: String,            -- ^ Warning message
  warnSuggestion :: Maybe String    -- ^ Optional suggestion for fixing the warning
}

-- | Types of errors that can occur during proof verification
data ErrorKind =
  -- Basic error types
    TypeError String                  -- ^ Type mismatch in proof
  | SyntaxError String                -- ^ Syntax error in input
  | RuleNotFoundError String          -- ^ Referenced rule not found
  | RuleConcError String              -- ^ Error in rule conclusion
  | RuleArgError Integer String       -- ^ Error in rule argument
  | RuleArgCountError Integer Integer -- ^ Wrong number of arguments for rule

  -- Advanced error types
  | MismatchedFormula Formula Formula -- ^ Formula mismatch (expected vs actual)
  | ReferenceError Ref String         -- ^ Error with a reference
  | OutOfScopeReference Ref           -- ^ Reference is out of scope
  | RuleApplicationError String Formula -- ^ Error applying a rule to a formula
  | MalformedProof String             -- ^ Structurally invalid proof
  | IncompleteProofError Formula      -- ^ Proof doesn't reach required conclusion
  | UnknownError String               -- ^ Unclassified error
  deriving (Show)

-- | A structured error with location, context, and suggestions
data Error = Error {
  errLocation :: Maybe Ref,          -- ^ Optional location of the error
  errKind :: ErrorKind,              -- ^ Type of error
  errMessage :: String,              -- ^ Error message
  errContext :: Maybe String,        -- ^ Additional context for the error
  errSuggestions :: [String]         -- ^ Possible fixes for the error
}

-- | Result type for computations that can fail with an error or succeed with warnings
data Result a = 
    Ok [Warning] a               -- ^ Success with optional warnings
  | Err [Warning] Env Error      -- ^ Failure with error, environment state, and optional warnings

----------------------------------------------------------------------
-- Environment
----------------------------------------------------------------------

-- | A reference to a position in a proof, either a line or range
data Ref = RefRange Integer Integer | RefLine Integer deriving (Eq, Ord)

-- | Type information for identifiers
data IDType = 
    IDTypeTerm Integer      -- ^ A term with arity
  | IDTypePred Integer      -- ^ A predicate with arity

-- | The environment for the proof system, containing all context needed for verification
data Env = Env {
    prems      :: Formulas,        -- ^ Premises/assumptions in the current scope
    depth      :: Integer,         -- ^ Current nesting depth of proofs
    fresh      :: Terms,           -- ^ Fresh variables in the current scope
    refs       :: Map Ref ArgTup,  -- ^ References to labeled steps in the proof
    rules      :: Map String (Env -> [ArgTup] -> Formula -> Result Formula), -- ^ Built-in inference rules
    user_rules :: Map String UDefRule, -- ^ User-defined rules
    pos        :: Refs,            -- ^ Current position in the proof (for error reporting)
    rule       :: String,          -- ^ Current rule being applied
    bound      :: Map String (),   -- ^ Variables that are bound in the current scope
    ids        :: Map String IDType -- ^ Mapping of identifiers to their types
}

----------------------------------------------------------------------
-- Type class instances
----------------------------------------------------------------------

instance Show Term where
    show (Term name []) = name
    show (Term name terms) = name ++ "(" ++ show terms ++ ")"

instance Eq Term where
    Term a as == Term b bs = a == b && as == bs

instance Show Predicate where
    show (Predicate name []) = name
    show (Predicate name terms) = name ++ "(" ++ show terms ++ ")"

instance Eq Predicate where
    (==) :: Predicate -> Predicate -> Bool
    Predicate a as == Predicate b bs = a == b && as == bs

instance Show Formula where
    show (Pred a) = show a
    show (And a b) = show a ++ " ∧ " ++ show b
    show (Or a b) = show a ++ " ∨ " ++ show b
    show (Impl a b) = show a ++ " → " ++ show b
    show (Eq a b) = show a ++ " = " ++ show b
    show (All x a) = "∀" ++ show x ++ " " ++ show a
    show (Some x a) = "∃" ++ show x ++ " " ++ show a
    show (Not a) = "¬" ++ show a
    show Bot = "⊥"
    show Nil = "Nil"


-- | Deprecated: does not fit our implementation and only used by Eq Proof

instance Eq Formula where
    Pred a == Pred b = a == b
    And a1 a2 == And b1 b2 = a1 == b1 && a2 == b2
    Or a1 a2 == Or b1 b2 = a1 == b1 && a2 == b2
    Impl a1 a2 == Impl b1 b2 = a1 == b1 && a2 == b2
    Eq a1 a2 == Eq b1 b2 = a1 == b1 && a2 == b2

    All x a == All y b =
        (x == y && a == b) ||
        (case isFreeFor env y x b of
            Ok _ True -> case replaceInFormula env x y b of
                Ok _ replaced -> a == replaced
                _ -> False
            _ -> False)
      where env = newEnv

    Some x a == Some y b =
        (x == y && a == b) ||
        (case isFreeFor env y x b of
            Ok _ True -> case replaceInFormula env x y b of
                Ok _ replaced -> a == replaced
                _ -> False)
      where env = newEnv

    Not a == Not b = a == b
    Bot == Bot = True
    Nil == Nil = True
    _ == _ = False

instance Show Proof where
    show (Proof [] premises conc) = show premises ++ " ⊢ " ++ show conc
    show (Proof terms premises conc) = show terms ++ show premises ++ " ⊢ " ++ show conc

instance Eq Proof where
    (==) :: Proof -> Proof -> Bool
    Proof terms1 prems1 conc1 == Proof terms2 prems2 conc2 = 
        terms1 == terms2 && prems1 == prems2 && conc1 == conc2

instance Show Arg where
    show (ArgProof p) = show p
    show (ArgForm f) = show f
    show (ArgTerm t) = show t
    show (ArgFormWith x phi) = "φ(" ++ show x ++ ") ≡ " ++ show phi

instance Show Ref where
    show (RefRange i j) = show i ++ "-" ++ show j
    show (RefLine i) = show i

instance Show Warning where
  show w = "[" ++ show (warnSeverity w) ++ "] " ++
           warnMessage w ++
           (case warnLocation w of
              Just loc -> " at " ++ show loc
              Nothing -> "") ++
           maybe "" ("\nSuggestion: " ++) (warnSuggestion w)

instance Show Error where
  show e = show (errKind e) ++
           (case errLocation e of
              Just loc -> " at " ++ show loc
              Nothing -> "") ++
           ": " ++ errMessage e ++
           maybe "" ("\nContext: " ++) (errContext e) ++
           (if null (errSuggestions e)
             then ""
             else "\nSuggestions:\n" ++ unlines (map ("- " ++) (errSuggestions e)))

instance Functor Result where
    -- | Map a function over the result value
    fmap :: (a -> b) -> Result a -> Result b
    fmap f (Ok warns x) = Ok warns (f x)
    fmap _ (Err warns env err) = Err warns env err

instance Applicative Result where
    -- | Lift a value into a successful result
    pure :: a -> Result a
    pure = Ok []
    
    -- | Apply a function in a Result to a value in a Result
    (<*>) :: Result (a -> b) -> Result a -> Result b
    (Ok warns1 f) <*> (Ok warns2 x) = Ok (warns1 ++ warns2) (f x)
    (Err warns env err) <*> _ = Err warns env err
    _ <*> (Err warns env err) = Err warns env err

instance Monad Result where
    -- | Return a value in a successful result
    return :: a -> Result a
    return = pure
    
    -- | Bind a computation to a function that returns a Result
    (>>=) :: Result a -> (a -> Result b) -> Result b
    (Ok warns x) >>= f = case f x of
        Ok newWarns y -> Ok (warns ++ newWarns) y
        Err newWarns env err -> Err (warns ++ newWarns) env err
    (Err warns env err) >>= _ = Err warns env err

----------------------------------------------------------------------
-- Error creation functions
----------------------------------------------------------------------

createNoRuleProvidedError :: Env -> String -> Error
createNoRuleProvidedError env message = Error {
  errLocation = listToMaybe (pos env),
  errKind = RuleNotFoundError message,
  errMessage = "Something went wrong with the rule application",
  errContext = Just message,
  errSuggestions = ["Check that the rule is defined and accessible"]
}

-- | Create an error for an invalid rule argument
createRuleArgError :: Env -> Integer -> String -> Error
createRuleArgError env argNum message = Error {
  errLocation = listToMaybe (pos env),
  errKind = RuleArgError argNum message,
  errMessage = "Invalid argument to rule",
  errContext = Just $ "Problem with argument #" ++ show argNum,
  errSuggestions = ["Check that you're using the correct formula type"]
}

-- | Create an error for incorrect number of arguments to a rule
createArgCountError :: Env -> Integer -> Integer -> Error
createArgCountError env actual expected = Error {
  errLocation = listToMaybe (pos env),
  errKind = RuleArgCountError actual expected,
  errMessage = "Incorrect number of arguments",
  errContext = Just $ "Expected " ++ show expected ++ " arguments, got " ++ show actual,
  errSuggestions = ["Check that you're using the correct rule for your situation"]
}

-- | Create an error for an invalid rule conclusion
createRuleConcError :: Env -> String -> Error
createRuleConcError env message = Error {
  errLocation = listToMaybe (pos env),
  errKind = RuleConcError message,
  errMessage = "Invalid conclusion for rule application",
  errContext = Just message,
  errSuggestions = ["Check that your conclusion matches what the rule is expected to produce"]
}

-- | Create a type mismatch error
createTypeError :: Env -> String -> Error
createTypeError env message = Error {
  errLocation = listToMaybe (pos env),
  errKind = TypeError message,
  errMessage = "Type mismatch in proof",
  errContext = Just message,
  errSuggestions = ["Ensure you're using the correct type of expressions"]
}

-- | Create an error for an invalid reference
createReferenceError :: Env -> Ref -> String -> Error
createReferenceError env ref message = Error {
  errLocation = Just ref,  -- Use the reference itself as the location
  errKind = ReferenceError ref message,
  errMessage = "Invalid reference in proof",
  errContext = Just $ "Problem with reference " ++ show ref,
  errSuggestions = ["Check that the reference exists and is accessible"]
}

-- | Create an error for a rule that wasn't found
createRuleNotFoundError :: Env -> String -> Error
createRuleNotFoundError env ruleName = Error {
  errLocation = listToMaybe (pos env),
  errKind = RuleNotFoundError ruleName,
  errMessage = "Rule not found: " ++ ruleName,
  errContext = Just $ "The rule '" ++ ruleName ++ "' is not defined",
  errSuggestions = [
    "Check for typos in the rule name",
    "Use one of the predefined rules",
    "Define this rule as a user-defined rule first"
  ]
}

-- | Create a general error for unexpected situations
createUnknownError :: Env -> String -> Error
createUnknownError env message = Error {
  errLocation = listToMaybe (pos env),
  errKind = UnknownError message,
  errMessage = "Unexpected error occurred",
  errContext = Just message,
  errSuggestions = []
}

-- | Create an error for formulas that don't match
createMismatchedFormulaError :: Env -> Formula -> Formula -> Error
createMismatchedFormulaError env expected actual = Error {
  errLocation = listToMaybe (pos env),
  errKind = MismatchedFormula expected actual,
  errMessage = "Formulas don't match",
  errContext = Just $ "Expected: " ++ show expected ++ "\nActual: " ++ show actual,
  errSuggestions = ["Verify that your formula matches the required pattern for this rule"]
}

-- | Create an error for syntax problems in the proof
createSyntaxError :: Env -> String -> Error
createSyntaxError env message = Error {
    errLocation = listToMaybe (pos env),
    errKind = SyntaxError message,
    errMessage = "Syntax error in proof",
    errContext = Just $ "Parser error: " ++ message,
    errSuggestions = [
        "Check for missing parentheses or brackets",
        "Ensure all formulas are properly terminated",
        "Look for typos in predicate or variable names"
    ]
}
