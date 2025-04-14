module Backend.Environment (
    Env,
    newEnv,
    push,
    bindVar,
    regTerm,
    addPrem,
    addFresh,
    getPrems,
    getFreshs,
    addRefs,
    getRefs,
    getRef,
    pushPos,
    popPos,
    applyRule,
    showPos,
    replaceInFormula,
    replaceInTerms,
    replaceInTerm,
    addUDefRule
) where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
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
    depth = 0,
    fresh = [],
    refs  = Map.empty,
    rules = Map.fromList [
        ("copy", ruleCopy),
        ("COPY", ruleCopy),
        ("C", ruleCopy),
        ("REITERATION", ruleCopy),
        ("R", ruleCopy),

        ("AndI", ruleAndI),
        ("&I", ruleAndI),
        ("∧I", ruleAndI),

        ("AndEL", ruleAndEL),
        ("&EL", ruleAndEL),
        ("∧EL", ruleAndEL),

        ("AndER", ruleAndER),
        ("&ER", ruleAndER),
        ("∧ER", ruleAndER),

        ("OrIL", ruleOrIL),
        ("|IL", ruleOrIL),
        ("∨IL", ruleOrIL),

        ("OrIR", ruleOrIR),
        ("|IR", ruleOrIR),
        ("∨IR", ruleOrIR),

        ("OrE", ruleOrE),
        ("|E", ruleOrE),
        ("∨E", ruleOrE),

        ("ImplI", ruleImplI),
        ("IfI", ruleImplI),
        ("->I", ruleImplI),
        ("→I", ruleImplI),

        ("ImplE", ruleImplE),
        ("IfE", ruleImplE),
        ("->E", ruleImplE),
        ("→E", ruleImplE),

        ("NotI", ruleNotI),
        ("!I", ruleNotI),
        ("¬I", ruleNotI),

        ("NotE", ruleNotE),
        ("!E", ruleNotE),
        ("¬E", ruleNotE),

        ("BotE", ruleBottomE),
        ("botE", ruleBottomE),
        ("#E", ruleBottomE),
        ("⊥E", ruleBottomE),

        ("NotNotI", ruleNotNotI),
        ("!!I", ruleNotNotI),
        ("¬¬I", ruleNotNotI),

        ("NotNotE", ruleNotNotE),
        ("!!E", ruleNotNotE),
        ("¬¬E", ruleNotNotE),

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
    user_rules = Map.empty,
    pos  = [],
    rule = "",
    bound = Map.empty,
    ids = Map.empty
}

addUDefRule :: Env -> String -> UDefRule -> Env
addUDefRule env name r = env{user_rules = Map.insert name r (user_rules env)}

showPos:: Env -> String
showPos env = if rule env == "" then p else p ++ ":" ++ r ++ " "
    where p = "[" ++ List.intercalate " " (reverse [show x| x <-  pos env]) ++ "]"
          r = rule env


-- Pushes a new context to the environment (used when entering a subproof or box).
-- This resets the list of premises for the new scope, keeping only assumptions.
-- Pushes a new context to the environment (used when entering a subproof or box).
-- This resets the list of premises for the new scope, keeping only assumptions.
push :: Env -> Env
push env = env { prems = [], fresh = [], depth = depth env + 1}


bindVar :: Env -> Term -> Result Env
bindVar env (Term x []) = if Map.member x (bound env)
    then Error [] env (UnknownError ("Trying to rebind "++ show x ++"."))
    else Ok [] env { bound = Map.insert x () (bound env)}
bindVar env _ = Error [] env (UnknownError "Unanble to bind an function.")

isFreeVar :: Env -> Term -> Result Bool
isFreeVar env (Term x []) = if Map.notMember x (bound env) then Ok [] True else Ok [] False
isFreeVar env _ = Error [] env (UnknownError "A function can not be a free variable.")

isBoundVar :: Env -> Term -> Result Bool
isBoundVar env (Term x []) = if Map.member x (bound env) then Ok [] True else Ok [] False
isBoundVar env _ = Error [] env (UnknownError "A function can not be a bound variable.")

isFreeFor :: Env -> Term -> Term -> Formula -> Result Bool
isFreeFor _ _ _ Bot = Ok [] True
isFreeFor _ _ _ (Pred _) = Ok [] True
isFreeFor _ _ _ (Eq _ _) = Ok [] True
isFreeFor env t x (And l r) = case isFreeFor env t x l of
    Error warns env_e err -> Error warns env_e err
    Ok warns1 res_l-> case isFreeFor env t x r of
        Error warns env_e err -> Error (warns++warns1) env_e err
        Ok warns2 res_r -> Ok (warns1++warns2) (res_l &&res_r)
isFreeFor env t x (Or l r) = case isFreeFor env t x l of
    Error warns env_e err -> Error warns env_e err
    Ok warns1 res_l-> case isFreeFor env t x r of
        Error warns env_e err -> Error (warns++warns1) env_e err
        Ok warns2 res_r -> Ok (warns1++warns2) (res_l &&res_r)
isFreeFor env t x (Impl l r) = case isFreeFor env t x l of
    Error warns env_e err -> Error warns env_e err
    Ok warns1 res_l-> case isFreeFor env t x r of
        Error warns env_e err -> Error (warns++warns1) env_e err
        Ok warns2 res_r -> Ok (warns1++warns2) (res_l &&res_r)
isFreeFor env t x (All y f) = case isFreeFor env t x f of
    Error warns env_e err -> Error warns env_e err
    Ok warns False -> Ok warns False 
    Ok warns True  ->  if Set.member x (freeVarForm (All y f)) && Set.notMember y (freeVarTerm t) then Ok warns True else Ok warns False
isFreeFor env t x (Some y f) = case isFreeFor env t x f of
    Error warns env_e err -> Error warns env_e err
    Ok warns False -> Ok warns False 
    Ok warns True  ->  if Set.member x (freeVarForm (Some y f)) && Set.notMember y (freeVarTerm t) then Ok warns True else Ok warns False
isFreeFor env t x (Not f) = isFreeFor env t x f
isFreeFor env _ _ Nil = Error [] env (UnknownError "Nil formula.")

freeVarTerm :: Term -> Set.Set Term
freeVarTerm (Term s []) = Set.singleton (Term s [])
freeVarTerm (Term _ [term]) = Set.singleton term
freeVarTerm (Term _ terms) = Set.unions [freeVarTerm term | term <- terms]

freeVarForm:: Formula -> Set.Set Term
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


regTerm :: Env -> Term -> Result Env
regTerm env (Term x param) = case Map.lookup x (ids env) of
    Nothing -> Ok [] env {ids = Map.insert x (IDTypeTerm n) (ids env)}
    Just  t -> case t of
         IDTypeTerm i -> if i == n 
            then Ok [] env 
            else Error [] env  (UnknownError ("Trying to redefine " ++ show x ++ "."))
         IDTypePred _ -> Error [] env  (UnknownError ("Trying to redefine " ++ show x ++ " as a term."))
    where n = toInteger (List.length param)

-- Section: Adders

addPrem :: Env -> Formula -> Result Env
addPrem env prem = Ok [] (env { prems = prems env ++ [prem] })

addFresh :: Env -> Term -> Result Env
addFresh env x@(Term _ []) = Ok [] env { fresh = fresh env ++ [x]}
addFresh env x = Error [] env (UnknownError ("Can not add function " ++ show x ++ " as a fresh variable."))

-- Adds references to the environment.
-- References map labels to arguments (e.g., proofs, formulas, or terms).
addRefs :: Env -> [Ref] -> Arg -> Env
addRefs env labels form = env { refs = Map.union (refs env) (Map.fromList [(label, (0, form)) | label <- labels])}
-- Section: Getters


-- Retrieves all premises/assumptions in the current scope.
getPrems :: Env -> [Formula]
getPrems = prems

getFreshs :: Env -> [Term]
getFreshs = fresh

-- Retrieves the values corresponding to a list of references.
-- Returns an error if any reference is invalid.
getRefs :: Env -> [Ref] -> Result (Env, [Arg])
getRefs env [] = Ok [] (env, [])
getRefs env (x : xs) =
    case getRefs env xs of
        Error warns env_e err -> Error warns env_e err
        Ok warns1 (env1, args) ->
            case getRef env1 x of
                Error warns env_e err -> Error (warns++warns1) env_e err
                Ok warns2 (env2, arg) -> Ok (warns1++warns2) (env2, arg : args)

-- Retrieves the value corresponding to a single reference.
-- Returns an error if the reference does not exist.
-- Make sure this function increments the reference count
getRef :: Env -> Ref -> Result (Env, Arg)
getRef env ref = case Map.lookup ref (refs env) of
    Nothing -> Error [] env (TypeError ("Reference " ++ show ref ++ " was not found."))
    Just (count, arg) -> Ok [] (env { refs = Map.insert ref (count + 1, arg) (refs env) }, arg)

pushPos :: Env -> [Ref] -> Env
pushPos env r = env {pos = r ++ pos env}

popPos :: Env -> Integer -> Env
popPos env n = env {pos = drop (fromIntegral n) (pos env)}
-- Applies a rule to a list of arguments and checks the result.
-- If the rule application succeeds, the resulting formula is returned.
-- Otherwise, an error is returned.
applyRule :: Env -> String -> [Arg] -> Formula -> Result Formula
applyRule e name args res =
    case Map.lookup name (rules env) of
        Just rule_f ->
            case rule_f env (zip [1..] args) res of
                Error warns env_e err -> Error warns env_e err
                Ok warns res_t ->
                    if res_t == res then Ok warns res_t
                    else Error warns env (RuleConcError ("Wrong conclusion when using rule, expected " ++ show res ++ ", got " ++ show res_t))
        Nothing -> case Map.lookup name (user_rules env) of
            Just rule_s -> case applyUDefRule env rule_s (zip [1..] args) of 
                Error warns env_e err -> Error warns env_e err
                Ok warns res_t ->
                    if res_t == res then Ok warns res_t
                    else Error warns env (RuleConcError ("Wrong conclusion when using rule, expected " ++ show res ++ ", got " ++ show res_t))
            Nothing -> Error [] env (RuleNotFoundError name)
    where env = e{rule=name}
-- All predefined rules.
-- Rules are functions that take:
--   - A list of arguments (e.g., proofs, formulas, or terms).
--   - An expected result formula.
--   - Return either the resulting formula or an error.

applyUDefRule:: Env -> UDefRule -> [(Integer, Arg)] -> Result Formula
applyUDefRule env sig@(UDefRule ins out) args = if (toInteger $ List.length ins) /= (toInteger $ List.length args) 
    then Error [] env (RuleArgCountError (toInteger $ List.length ins) (toInteger $ List.length args) )
    else do
        ph <- findPlaceholdersInArgs env ins args Map.empty
        replacePlaceholders env out ph

findPlaceholdersInArgs:: Env -> [Formula] -> [(Integer, Arg)] -> Map.Map Predicate Formula -> Result (Map.Map Predicate Formula)
findPlaceholdersInArgs env [] [] ph = Ok [] ph
findPlaceholdersInArgs env [a] [(i, b)] ph = case b of
    ArgForm b_f -> findPlaceholdersInArg env i a b_f ph
    _ -> Error [] env (RuleArgError i "Arguments in user defined rules must be formulas.")
findPlaceholdersInArgs env (a:as) ((i, b):bs) ph = case b of
    ArgForm b_f -> do
        ph1 <- findPlaceholdersInArg env i a b_f ph
        ph2 <- findPlaceholdersInArgs env as bs ph1
        Ok [] ph2
    _ -> Error [] env (RuleArgError i "Arguments in user defined rules must be formulas.")
findPlaceholdersInArgs env _ _ _ = Error [] env (UnknownError "Missmatch in number of args and signature.")

findPlaceholdersInArg:: Env -> Integer -> Formula -> Formula -> Map.Map Predicate Formula -> Result (Map.Map Predicate Formula)
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
    Just c -> if b == c  -- may need more complex test? 
        then Ok [] ph
        else Error [] env (RuleArgError i "Did not match previous instance of placeholder.")
findPlaceholdersInArg env i _ _ ph = Error [] env (UnknownError "Unimplemented.")

replacePlaceholders:: Env -> Formula -> Map.Map Predicate Formula -> Result Formula
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
    Nothing -> Error [] env (UnknownError "Unimplemented.")
    Just res -> Ok [] res
replacePlaceholders env _ _ = Error [] env (UnknownError "Unimplemented.")

ruleCopy :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleCopy _ [(_, ArgForm form)] _ = Ok [] form
ruleCopy env forms          _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleAndI :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleAndI _ [(_, ArgForm a), (_, ArgForm b)] _ = Ok [] (And a b)
ruleAndI env [_, (j, ArgForm _)]         _ = Error [] env (RuleArgError j "Needs to be a formula.")
ruleAndI env [(i, _), _]                 _ = Error [] env (RuleArgError i "Needs to be a formula.")
ruleAndI env forms                  _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 2 )

ruleAndEL :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleAndEL _ [(_ ,ArgForm (And l _))] _ = Ok [] l
ruleAndEL env [_]                 _ = Error [] env (RuleArgError 1 "Needs to be a and formula.")
ruleAndEL env forms               _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleAndER :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleAndER _ [(_, ArgForm (And _ r))] _ = Ok [] r
ruleAndER env [_]                 _ = Error [] env (RuleArgError 1 "Needs to be a and formula.")
ruleAndER env forms               _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleOrIL :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleOrIL env [(_, ArgForm a)]  r@(Or b _) = if a == b then Ok [] r else Error [] env (RuleArgError 1 "Did not match left hand side of conclusion.")
ruleOrIL env [(_, ArgForm _)]           _ = Error [] env (RuleConcError "Conclusion needs to be an or formula.")
ruleOrIL env [_]            (Or _ _) = Error [] env (RuleArgError 1 "Needs to be a and formula.")
ruleOrIL env forms                 _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleOrIR :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleOrIR env [(_, ArgForm a)]  r@(Or _ b) = if a == b then Ok [] r else Error [] env (RuleArgError 1 "did not match right hand side of conclusion.")
ruleOrIR env [(_, ArgForm _)]           _ = Error [] env (RuleConcError "Conclusion needs to be an or formula.")
ruleOrIR env [_]            (Or _ _) = Error [] env (RuleArgError 1 "needs to be a and formula.")
ruleOrIR env forms                 _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleOrE :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleOrE env [(_, ArgForm (Or a b)), (j, ArgProof (Proof _ [p1] c1)), (k, ArgProof (Proof _ [p2] c2))] _ =
    if a == p1 then
        if b == p2 then
            if c1 == c2 then Ok [] c1 else Error [] env (RuleConcError "The conclusions of the two proofs did not match.")--Not realy conclusion error? 
        else Error [] env (RuleArgError k "The premise of the proof did not match the right hand side of the or statement.")
    else Error [] env (RuleArgError j "The premise of the proof did not match the left hand side of the or statement.")
ruleOrE env [b@(_, ArgProof _), a@(_, ArgForm _), c@(_, ArgProof _)] r = ruleOrE env [a, b, c] r
ruleOrE env [b@(_, ArgProof _), c@(_, ArgProof _), a@(_, ArgForm _)] r = ruleOrE env [a, b, c] r
ruleOrE env [(_, ArgForm (Or _ _)), (_, ArgProof _), (k, _)] _ = Error [] env (RuleArgError k "Needs to be an proof.")
ruleOrE env [(_, ArgForm (Or _ _)), (j, _), _]          _ = Error [] env (RuleArgError j "Needs to be an proof.")
ruleOrE env [(i, _), _, _]                         _ = Error [] env (RuleArgError i "Needs to be an or formula.")
ruleOrE env forms                             _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 3 )

ruleImplI :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleImplI _ [(_, ArgProof (Proof _ [a] b))] _ = Ok [] (Impl a b)
ruleImplI env [_]                      _ = Error [] env (RuleArgError 1 "Needs to be an proof.")
ruleImplI env forms                    _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleImplE :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleImplE env [(_, ArgForm a), (j, ArgForm (Impl b c))] _ =
    if a == b then Ok [] c
    else Error [] env (RuleArgError j "Premise did not match argument 1.")
ruleImplE env [b@(_, ArgForm (Impl _ _)), a@(_, ArgForm _)] r = ruleImplE env [a,b] r
ruleImplE env [(_, ArgForm _), (j, _)]         _ = Error [] env (RuleArgError j "Needs to be an implication formula.")
ruleImplE env [(i, _)        , _]         _ = Error [] env (RuleArgError i "Needs to be a formula.")
ruleImplE env forms                  _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 2 )

ruleNotI :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleNotI _ [(_, ArgProof (Proof _ [a] Bot))] _ = Ok [] (Not a)
ruleNotI env [_]                        _ = Error [] env (RuleArgError 1 "Needs to be a proof with the conclusion of bot.")
ruleNotI env forms                      _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleNotE :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleNotE env [(i, ArgForm a), (j, ArgForm (Not b))] _ = if a == b then Ok [] Bot else Error [] env (TypeError ("Argument "++ show j ++" is not the negation of argument "++show i++"."))
ruleNotE env [b@(_, ArgForm (Not _)), a@(_, ArgForm _)] r = ruleNotE env [a, b] r;
ruleNotE env [(_, ArgForm _), (j, _)]               _ = Error [] env (RuleArgError j "Needs to be a not formula.")
ruleNotE env [(i, _), _]                       _ = Error [] env (RuleArgError i "Needs to be a formula.")
ruleNotE env forms                        _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 2 )

ruleBottomE :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleBottomE _ [(_, ArgForm Bot)] r = Ok [] r
ruleBottomE env [_]           _ = Error [] env (RuleArgError 1 "Needs to be a bottom formula.")
ruleBottomE env forms         _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleNotNotI :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleNotNotI _ [(_, ArgForm a)] _ = Ok [] (Not (Not a))
ruleNotNotI env [_]         _ = Error [] env (RuleArgError 1 "Needs to be a formula.")
ruleNotNotI env forms       _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleNotNotE :: Env -> [(Integer, Arg)] -> Formula -> Result Formula
ruleNotNotE _ [(_, ArgForm (Not (Not a)))] _ = Ok [] a
ruleNotNotE env [_]                     _ = Error [] env (RuleArgError 1 "Needs to be a not not formula.")
ruleNotNotE env forms                   _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

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
ruleEqE env [(i, ArgForm (Eq t1 t2)), (j, ArgForm a), (_, ArgFormWith u phi)] _ = case isFreeFor env t1 u phi of
    Error warns env_e err -> Error warns env_e err
    Ok warns_t1 False -> Error warns_t1 env (RuleArgError i (show t1 ++ "needs to be free for " ++ show u ++" in " ++ show phi))
    Ok warns_t1 True -> case isFreeFor env t2 u phi of
        Error warns env_e err -> Error warns env_e err
        Ok warns_t2 False -> Error (warns_t1++warns_t2) env (RuleArgError i (show t2 ++ "needs to be free for " ++ show u ++" in " ++ show phi))
        Ok warns_t2 True  -> case replaceInFormula env u t1 phi of
            Error warns env_e err -> Error (warns++warns_t1++warns_t2) env_e err
            Ok warns1 b -> if a /= b then Error (warns_t1++warns_t2++warns1) env (RuleArgError j ("Do not match "++show phi++"["++ show t1 ++"/"++ show u ++"].")) else
                case replaceInFormula env u t2 phi of
                    Error warns env_e err -> Error (warns++warns_t1++warns_t2++warns1) env_e err
                    Ok warns2 c -> Ok (warns_t1++warns_t2++warns1++warns2) c
ruleEqE env [(_, ArgForm (Eq _ _)), (_, ArgForm _), (k, _)] _ = Error [] env (RuleArgError k  "Must be an formula.")
ruleEqE env [(_, ArgForm (Eq _ _)), (j, _), (_, _)] _ = Error [] env (RuleArgError j  "Must be an formula.")
ruleEqE env [(i, _), (_, _), (_, _)] _ = Error [] env (RuleArgError i  "Must be an equality.")
ruleEqE env forms _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 3 )

ruleAllE:: Env-> [(Integer, Arg)] -> Formula -> Result Formula
ruleAllE env [(_, ArgForm (All x phi)), (j, ArgTerm t)] _ =  case isFreeFor env t x phi of
    Error warns env_e err -> Error warns env_e err
    Ok warns False -> Error warns env (RuleArgError j (show t ++ "needs to be free for " ++ show x ++" in " ++ show phi))
    Ok warns1 True -> case replaceInFormula env x t phi of 
        Error warns env_e err -> Error (warns++warns1) env_e err
        Ok warns2 res -> Ok (warns1++warns2) res
ruleAllE env [(_, ArgForm (All _ _)), (j, _)] _ = Error [] env (RuleArgError j  "Must be an free variable.")
ruleAllE env [(i, _), (_, _)] _ = Error [] env (RuleArgError i  "Must be an for all formula.")
ruleAllE env forms _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 2 )

ruleAllI:: Env-> [(Integer, Arg)] -> Formula -> Result Formula
ruleAllI env [(_, ArgProof (Proof [t] [] a))] (All x b) = case replaceInFormula env x t b of
    Error warns env_e err -> Error warns env_e err
    Ok warns c -> if a==c then Ok warns (All x b) else Error [] env (RuleConcError "The given formula did not match the conclusion.")
ruleAllI env [(_, ArgProof (Proof [_] [] _))] _ = Error [] env (RuleConcError "The conclusion must be an for all formula.")
ruleAllI env [(i, _)] _ = Error [] env (RuleArgError i  "Must be an box.")
ruleAllI env forms _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 1 )

ruleSomeE:: Env-> [(Integer, Arg)] -> Formula -> Result Formula --TODO do we need to check if b mentions x_0?
ruleSomeE env [(_,ArgForm (Some x a)), (_, ArgProof(Proof [x_0] [b] c))] _ = case replaceInFormula env x x_0 a of
    Error warns env_e err -> Error warns env_e err
    Ok warns d -> if b == d then Ok [] c else Error warns env (RuleConcError (show a ++ "[" ++ show x_0 ++ "/" ++show x ++ "] resulted in " ++ show d ++ " and not " ++ show b ++" as expected." ))
ruleSomeE env [(_,ArgForm (Some _ _)), (j, b)] _ = Error [] env (RuleArgError j  ("Must be an proof not "++ show b ++"."))
ruleSomeE env [(i,a), (_, _)] _ = Error [] env (RuleArgError i  ("Must be an Exists formula not "++ show a ++ "."))
ruleSomeE env forms _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 2 )

ruleSomeI:: Env-> [(Integer, Arg)] -> Formula -> Result Formula
ruleSomeI env [(_, ArgForm a),  (j, ArgTerm t@(Term _ []))] (Some x phi) = case isFreeFor env t x phi of
    Error warns env_e err -> Error warns env_e err
    Ok warns False -> Error warns env (RuleArgError j (show t ++ "needs to be free for " ++ show x ++" in " ++ show phi))
    Ok warns1 True -> case replaceInFormula env x t phi of
        Error warns env_e err -> Error warns env_e err
        Ok warns2 c -> if a == c then Ok (warns1++warns2) (Some x phi) else Error [] env (RuleConcError (show phi ++ "[" ++ show x ++ "/" ++show t ++ "] resulted in " ++ show c ++ " and not " ++ show a ++" as expected." ))
ruleSomeI env [(_, ArgForm _),  (_, ArgTerm (Term _ []))] r = Error [] env (RuleConcError ("The conclusion must be an exist formula not " ++ show r ++"."))
ruleSomeI env [(_, ArgForm _), (j, _)] _ = Error [] env (RuleArgError j  "Must be an free variable.")
ruleSomeI env [(i, _), (_, _)] _ = Error [] env (RuleArgError i  "Must be an formula.")
ruleSomeI env forms _ = Error [] env (RuleArgCountError (toInteger $ List.length forms) 2 )

replaceInTerm:: Env -> Term -> Term -> Term -> Result Term
replaceInTerm env var@(Term x []) t a@(Term y []) = case isFreeVar env var of
    Error warns env_e err -> Error warns env_e err
    Ok warns is_free -> if is_free && (x == y) then Ok warns t else Ok warns a
replaceInTerm env x@(Term _ []) t (Term f args) = case replaceInTerms env x t args of
    Error warns env_e err -> Error warns env_e err
    Ok warns new_args -> Ok warns (Term f new_args)
replaceInTerm env _ _ _ = Error [] env (UnknownError "needs to be a variable.")

--Replace each free occurrens of the variable x with the term t in a list of terms
replaceInTerms:: Env -> Term -> Term -> [Term] -> Result [Term]
replaceInTerms _ (Term _ []) _ [] = Ok [] []
replaceInTerms env x@(Term _ []) t (arg : args) = case replaceInTerms env x t args of
    Error warns env_e err -> Error warns env_e err
    Ok warns1 args_new -> case replaceInTerm env x t arg of
        Error warns env_e err -> Error (warns++warns1) env_e err
        Ok warns2 arg_new -> Ok (warns1++warns2) (arg_new : args_new)
replaceInTerms env _ _ _ = Error [] env (UnknownError "needs to be a variable.")

--Replace each free occurrens of the variable x with the term t in the formula f f[t/x]
replaceInFormula:: Env -> Term -> Term -> Formula -> Result Formula
replaceInFormula _ (Term _ []) _ Bot = Ok [] Bot
replaceInFormula _ (Term _ []) _ p@(Pred (Predicate _ [])) = Ok [] p
replaceInFormula env x@(Term _ []) t (Pred (Predicate p args)) = case replaceInTerms env x t args of
    Error warns env_e err -> Error warns env_e err
    Ok warns args_new -> Ok warns (Pred (Predicate p args_new))
replaceInFormula env x@(Term _ []) t (Eq l r) = case replaceInTerm env x t l of
    Error warns env_e err -> Error warns env_e err
    Ok warns_l l_new -> case replaceInTerm env x t r of
        Error warns env_e err -> Error (warns_l++warns) env_e err
        Ok warns_r r_new -> Ok (warns_l++warns_r) (Eq l_new r_new)
replaceInFormula env x@(Term _ []) t (And l r) = case replaceInFormula env x t l of
    Error warns env_e err -> Error warns env_e err
    Ok warns_l l_new -> case replaceInFormula env x t r of
        Error warns env_e err -> Error (warns_l++warns) env_e err
        Ok warns_r r_new -> Ok (warns_l++warns_r) (And l_new r_new)
replaceInFormula env x@(Term _ []) t (Or l r) = case replaceInFormula env x t l of
    Error warns env_e err -> Error warns env_e err
    Ok warns_l l_new -> case replaceInFormula env x t r of
        Error warns env_e err -> Error (warns_l++warns) env_e err
        Ok warns_r r_new -> Ok (warns_l++warns_r) (Or l_new r_new)
replaceInFormula env x@(Term _ []) t (Impl l r) = case replaceInFormula env x t l of
    Error warns env_e err -> Error warns env_e err
    Ok warns_l l_new -> case replaceInFormula env x t r of
        Error warns env_e err -> Error (warns_l++warns) env_e err
        Ok warns_r r_new -> Ok (warns_l++warns_r) (Impl l_new r_new)
replaceInFormula env x@(Term _ []) t (All y a) = if x == y then Ok [] (All y a) 
    else case isFreeFor env t x (All y a) of
        Error warns env_e err -> Error warns env_e err
        Ok warns_free False -> Error warns_free env (UnknownError "Cannot replace variable")
        Ok warns_free True -> case bindVar env y of
            Error warns env_e err -> Error (warns++warns_free) env_e err
            Ok warns_b new_env -> case replaceInTerm new_env x t y of
                Error warns_y env_e err -> Error (warns_free++warns_b++warns_y) env_e err
                Ok warns_y y_new -> case replaceInFormula new_env x t a of
                    Error warns_a env_e err -> Error (warns_free++warns_y++warns_a++warns_b) env_e err
                    Ok warns_a a_new -> Ok (warns_free++warns_y++warns_a++warns_b) (All y_new a_new)
replaceInFormula env x@(Term _ []) t (Some y a) =  if x == y then Ok [] (Some y a) 
    else case isFreeFor env t x (Some y a) of
        Error warns env_e err -> Error warns env_e err
        Ok warns_free False -> Error warns_free env (UnknownError "Cannot replace variable")
        Ok warns_free True -> case bindVar env y of
            Error warns env_e err -> Error (warns++warns_free) env_e err
            Ok warns_b new_env -> case replaceInTerm new_env x t y of
                Error warns_y env_e err -> Error (warns_free++warns_b++warns_y) env_e err
                Ok warns_y y_new -> case replaceInFormula new_env x t a of
                    Error warns_a env_e err -> Error (warns_free++warns_y++warns_a++warns_b) env_e err
                    Ok warns_a a_new -> Ok (warns_free++warns_y++warns_a++warns_b) (Some y_new a_new)
replaceInFormula env  x@(Term _ []) t (Not a) = case replaceInFormula env x t a of
    Error warns env_e err -> Error warns env_e err
    Ok warns a_new -> Ok warns (Not a_new)
replaceInFormula env (Term _ []) _ Nil = Error [] env (UnknownError "trying to do an replace on nill formula.")
replaceInFormula env x _ _ = Error [] env (UnknownError (show x ++ " needs to be a variable."))
