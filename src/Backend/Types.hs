{-# LANGUAGE InstanceSigs #-}
module Backend.Types (
    Ref(RefRange, RefLine),
    Arg(ArgProof, ArgForm, ArgTerm),
    Proof(Proof),
    Formula(Pred, And, Or, If , Eq, All, Some, Not, Bot, Nil),
    Predicate(Predicate),
    Term(Term),
    Result(Ok, Error),
    ErrorKind(TypeError, SyntaxError, UnknownError),
    Env(..),
    replaceTerm

) where

import Data.Map as Map

type Terms = [Term]

data Env = Env {
      prems  :: [Formula]
    , refs   :: Map.Map Ref Arg
    , rules  :: Map.Map String (Env -> [Arg] -> Formula -> Result Formula)
    , consts :: Terms
    , vars   :: Terms
    , funs   :: Terms
}

-- Represents the result of an operation, which can either succeed (Ok) or fail (Error).
data Result a = Ok a | Error ErrorKind String

-- Represents a reference in a proof, either a range or a single line.
data Ref = RefRange Integer Integer | RefLine Integer deriving (Show, Eq, Ord)

-- Represents an argument in a proof, which can be a proof, formula, or term.
data Arg = ArgProof Proof | ArgForm Formula | ArgTerm Term
instance Show Arg where 
    show (ArgProof p) = show p 
    show (ArgForm f) = show f 

-- Represents a proof with premises and a conclusion.
data Proof = Proof [Formula] Formula
instance Show Proof where
    show (Proof prems conc) = show prems ++ "|-" ++ show conc
instance Eq Proof where 
    Proof prems1 conc1 == Proof prems2 conc2 = prems1 == prems2 && conc1 == conc2

-- Represents a logical formula.
data Formula = 
    Pred Predicate |
    And Formula Formula |
    Or Formula Formula |
    If Formula Formula |
    Eq Term Term |
    All Term Formula |
    Some Term Formula |
    Not Formula |
    Bot |
    Nil
instance Show Formula where
    show (Pred a) = show a
    show (And a b) = show a ++ "&" ++ show b
    show (Or a b) = show a ++ "|" ++ show b
    show (If a b) = show a ++ "->" ++ show b
    show (Eq a b) = show a ++ "=" ++ show b
    show (Not a) = "!" ++ show a
    show Bot = "bot"
    show Nil = "Nil"
instance Eq Formula where 
    Pred a == Pred b = a == b
    And a1 a2 == And b1 b2 = a1 == b1 && a2 == b2
    Or a1 a2 == Or b1 b2 = a1 == b1 && a2 == b2
    If a1 a2 == If b1 b2 = a1 == b1 && a2 == b2
    Eq a1 a2 == Eq b1 b2 = a1 == b1 && a2 == b2 
    Not a == Not b = a == b
    Bot == Bot = True
    Nil == Nil = True
    _ == _ = False

-- Represents a predicate with a name and a list of terms.
data Predicate = Predicate String [Term]
instance Show Predicate where
    show (Predicate name []) = name
    show (Predicate name terms) = name ++ "(" ++ show terms ++ ")"
instance Eq Predicate where 
    (==) :: Predicate -> Predicate -> Bool
    Predicate a as == Predicate b bs = a == b && as == bs

-- Represents a term with a name and a list of subterms.
data Term = Term String [Term]
instance Show Term where
    show (Term name []) = name
    show (Term name terms) = name ++ "(" ++ show terms ++ ")"
instance Eq Term where 
    Term a as == Term b bs = a == b && as == bs

-- Represents the kinds of errors that can occur.
data ErrorKind = TypeError | SyntaxError | UnknownError deriving Show 

-- Replaces all instances of a term `t` in a formula `f` with another term `x`.
replaceTerm :: Term -> Term -> Formula -> Formula
replaceTerm t x f = case f of
    Pred p -> Pred p
    And l r -> And (replaceTerm t x l) (replaceTerm t x r)
    Or l r -> Or (replaceTerm t x l) (replaceTerm t x r)
    If l r -> If (replaceTerm t x l) (replaceTerm t x r)
    Eq l r -> Eq (if l == t then x else l) (if r == t then x else r)
    Not a -> Not (replaceTerm t x a)
    Bot -> Bot
    Nil -> Nil