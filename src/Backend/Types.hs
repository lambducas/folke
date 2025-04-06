{-# LANGUAGE InstanceSigs #-}
module Backend.Types (
    Ref(RefRange, RefLine),
    Arg(..),
    Proof(Proof),
    Formula(Pred, And, Or, Impl , Eq, All, Some, Not, Bot, Nil),
    Predicate(Predicate),
    Term(Term),
    Result(Ok, Error),
    Warning,
    ErrorKind(..),
    Env(..)
) where

import Data.Map as Map ( Map )

type Terms  = [Term]

data Env = Env {
      prems  :: [Formula]
    , frees  :: Terms
    , refs   :: Map.Map Ref (Integer, Arg)
    , rules  :: Map.Map String (Env -> [(Integer, Arg)] -> Formula -> Result Formula)
    , pos    :: [Ref]
    , rule   :: String
    , bound  :: Map.Map String ()
}

-- Represents the result of an operation, which can either succeed (Ok) or fail (Error).
data Result t = Ok [Warning] t | Error [Warning] Env ErrorKind 

data Warning = Warning Env String

-- Represents the kinds of errors that can occur.
data ErrorKind = 
    TypeError String | 
    SyntaxError String | 
    RuleNotFoundError String |
    RuleConcError String |
    RuleArgError Integer String |
    RuleArgCountError Integer Integer |
    UnknownError String

instance Show ErrorKind where 
    show (TypeError msg) = "TypeError: "++msg 
    show (SyntaxError msg) = "SyntaxError: "++msg
    show (RuleNotFoundError name) = "RuleNotFoundError: No rule named " ++ name ++ " exists."
    show (RuleConcError msg) = "RuleConcError: Error in conclusion: " ++ msg 
    show (RuleArgError arg msg) = "RuleArgError: Error in argument "++ show arg ++ " : "++ msg
    show (RuleArgCountError arg_c arg_e ) = "RuleArgError: To " ++ (if arg_c < arg_e then "few" else "many") ++ " arguments expected " ++ show arg_e ++ " not " ++ show arg_c ++ "." 
    show (UnknownError msg) = "UnknownError: "++msg

-- Represents a reference in a proof, either a range or a single line.
data Ref = RefRange Integer Integer | RefLine Integer deriving (Eq, Ord)
instance Show Ref where 
    show (RefRange i j) = show i ++ "-" ++ show j
    show (RefLine i) = show i

-- Represents an argument in a proof, which can be a proof, formula, or term.
data Arg = ArgProof Proof | ArgForm Formula | ArgTerm Term | ArgFormWith Term Formula
instance Show Arg where 
    show (ArgProof p) = show p 
    show (ArgForm f) = show f 
    show (ArgTerm t) = show t
    show (ArgFormWith x phi) = "φ("++show x++")≡" ++show phi 

-- Represents a proof with premises and a conclusion.
data Proof = Proof [Term] [Formula] Formula
instance Show Proof where
    show (Proof [] premises conc) = show premises ++ "⊢" ++ show conc
    show (Proof terms premises conc) = show terms ++ show premises ++ "⊢" ++ show conc
instance Eq Proof where 
    (==) :: Proof -> Proof -> Bool
    Proof terms1 prems1 conc1 == Proof terms2 prems2 conc2 = terms1 == terms2 && prems1 == prems2 && conc1 == conc2

-- Represents a logical formula.
data Formula = 
    Pred Predicate |
    And Formula Formula |
    Or Formula Formula |
    Impl Formula Formula |
    Eq Term Term |
    All Term Formula |
    Some Term Formula |
    Not Formula |
    Bot |
    Nil
instance Show Formula where
    show (Pred a) = show a
    show (And a b) = show a ++ "∧" ++ show b
    show (Or a b) = show a ++ "∨" ++ show b
    show (Impl a b) = show a ++ "→" ++ show b
    show (Eq a b) = show a ++ "=" ++ show b
    show (All x a) = "∀" ++ show x ++ " " ++  show a
    show (Some x a) = "∃" ++ show x ++ " " ++  show a
    show (Not a) = "¬" ++ show a
    show Bot = "⊥"
    show Nil = "Nil"
instance Eq Formula where 
    Pred a == Pred b = a == b
    And a1 a2 == And b1 b2 = a1 == b1 && a2 == b2
    Or a1 a2 == Or b1 b2 = a1 == b1 && a2 == b2
    Impl a1 a2 == Impl b1 b2 = a1 == b1 && a2 == b2
    Eq a1 a2 == Eq b1 b2 = a1 == b1 && a2 == b2
    All x a == All y b = x == y && a == b 
    Some x a == Some y b = x == y && a == b 
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