{-# LANGUAGE InstanceSigs #-}
module Backend.Types (
    Ref(RefRange, RefLine),
    Arg(ArgProof, ArgForm, ArgTerm),
    Proof(Proof),
    Formula(Pred, And, Or, If , Eq, All, Some, Not, Bot, Nil),
    Predicate(Predicate),
    Term(Term),
    Result(Ok, Error),
    ErrorKind(TypeError,SyntaxError, UnknownError),
    replaceTerm
) where
data Result a = Ok a | Error ErrorKind String

data Ref = RefRange Integer Integer | RefLine Integer deriving (Show, Eq, Ord)

data Arg = ArgProof Proof | ArgForm Formula | ArgTerm Term
instance Show Arg where 
    show (ArgProof p) = show p 
    show (ArgForm f) = show f 

data Proof = Proof [Formula] Formula
instance Show Proof where
    show (Proof prems conc) = show prems ++ "|-" ++ show conc
instance Eq Proof where 
    Proof prems1 conc1 == Proof prems2 conc2 = prems1 == prems2 && conc1 == conc2

data Formula = 
            Pred Predicate |
            And  Formula Formula |
            Or   Formula Formula |
            If   Formula Formula |
            Eq   Term Term |
            All Term Formula |
            Some Term Formula |
            Not  Formula |
            Bot  |
            Nil
instance Show Formula where
    show (Pred a) = show a
    show (And a b) = show a ++ "&" ++ show b
    show (Or a b) = show a ++ "|" ++ show b
    show (If a b) = show a ++ "->" ++ show b
    show (Eq a b) = show a ++ "=" ++ show b
    show (Not a) = "!" ++ show a
    show Bot  = "bot"
    show Nil = "Nil"
instance Eq Formula where 
    Pred a == Pred b = a==b
    And a1 a2 == And b1 b2 = a1 == b1 && a2 == b2
    Or a1 a2 == Or b1 b2 = a1 == b1 && a2 == b2
    If a1 a2 == If b1 b2 = a1 == b1 && a2 == b2
    Eq a1 a2 == Eq b1 b2 = a1 == b1 && a2 == b2 
    Not a == Not b = a==b
    Bot == Bot = True
    Nil == Nil = True
    _ == _ = False


data Predicate = Predicate String [Term]
instance Show Predicate where
    show (Predicate name []) = name
    show (Predicate name terms) = name ++ "("++ show terms ++")"
instance Eq Predicate where 
    (==) :: Predicate -> Predicate -> Bool
    Predicate a as == Predicate b bs = a == b && as == bs

data Term = Term String [Term]
instance Show Term where
    show (Term name []) = name
    show (Term name terms) = name ++ "("++ show terms ++")"
instance Eq Term where 
    Term a as == Term b bs = a == b && as == bs


data ErrorKind = TypeError | SyntaxError | UnknownError deriving Show 

{-
    replaces all instances of term t in f with the variable x
    -params:
        - t
        - x
        - f
    -return: f[t/x]
-}
replaceTerm:: Term -> Term -> Formula -> Formula
replaceTerm t x f = case f of
    Pred p -> Pred p --Check parameters
    And l r -> And (replaceTerm t x l) (replaceTerm t x r)
    Or l r -> Or (replaceTerm t x l) (replaceTerm t x r)
    If l r -> If (replaceTerm t x l) (replaceTerm t x r)
    Eq l r -> Eq (if l == t then x else l) (if r == t then x else r)--Check parameters
    Not a -> Not (replaceTerm t x a)
    Bot -> Bot
    Nil -> Nil