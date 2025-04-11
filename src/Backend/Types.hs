{-# LANGUAGE InstanceSigs #-}

module Backend.Types (
    Ref(RefRange, RefLine),
    Arg(..),
    Proof(Proof),
    Formula(..),
    Predicate(Predicate),
    Term(Term),
    Result(..),
    Warning(Warning),
    ErrorKind(..),
    Env(..),
    IDType(..)
) where

import Data.Map as Map (Map)

type Terms = [Term]
type Refs = [Ref]

data Env = Env {
    prems   :: [Formula],
    depth   :: Integer,
    fresh   :: Terms,
    refs    :: Map Ref (Integer, Arg),
    rules   :: Map String (Env -> [(Integer, Arg)] -> Formula -> Result Formula),
    pos     :: Refs,
    rule    :: String,
    bound   :: Map String (),
    ids     :: Map String IDType
}

data Result t = Ok [Warning] t | Error [Warning] Env ErrorKind 

instance Functor Result where
    fmap :: (a -> b) -> Result a -> Result b
    fmap f (Ok warns x) = Ok warns (f x)
    fmap _ (Error warns env err) = Error warns env err

instance Applicative Result where
    pure :: a -> Result a
    pure = Ok []
    (<*>) :: Result (a -> b) -> Result a -> Result b
    (Ok warns1 f) <*> (Ok warns2 x) = Ok (warns1 ++ warns2) (f x)
    (Error warns env err) <*> _ = Error warns env err
    _ <*> (Error warns env err) = Error warns env err

instance Monad Result where
    return :: a -> Result a
    return = pure
    (>>=) :: Result a -> (a -> Result b) -> Result b
    (Ok warns x) >>= f = case f x of
        Ok newWarns y -> Ok (warns ++ newWarns) y
        Error newWarns env err -> Error (warns ++ newWarns) env err
    (Error warns env err) >>= _ = Error warns env err

data Warning = Warning Env String

data ErrorKind = 
    TypeError String | 
    SyntaxError String | 
    RuleNotFoundError String |
    RuleConcError String |
    RuleArgError Integer String |
    RuleArgCountError Integer Integer |
    UnknownError String

instance Show ErrorKind where 
    show (TypeError msg) = "TypeError: " ++ msg 
    show (SyntaxError msg) = "SyntaxError: " ++ msg
    show (RuleNotFoundError name) = "RuleNotFoundError: No rule named " ++ name ++ " exists."
    show (RuleConcError msg) = "RuleConcError: " ++ msg 
    show (RuleArgError arg msg) = "RuleArgError: Argument " ++ show arg ++ " - " ++ msg
    show (RuleArgCountError arg_c arg_e) = "RuleArgCountError: Expected " ++ show arg_e ++ ", got " ++ show arg_c ++ "."
    show (UnknownError msg) = "UnknownError: " ++ msg

data Ref = RefRange Integer Integer | RefLine Integer deriving (Eq, Ord)
instance Show Ref where 
    show (RefRange i j) = show i ++ "-" ++ show j
    show (RefLine i) = show i

data Arg = ArgProof Proof | ArgForm Formula | ArgTerm Term | ArgFormWith Term Formula
instance Show Arg where 
    show (ArgProof p) = show p 
    show (ArgForm f) = show f 
    show (ArgTerm t) = show t
    show (ArgFormWith x phi) = "φ(" ++ show x ++ ") ≡ " ++ show phi 

data Proof = Proof [Term] [Formula] Formula
instance Show Proof where
    show (Proof [] premises conc) = show premises ++ " ⊢ " ++ show conc
    show (Proof terms premises conc) = show terms ++ show premises ++ " ⊢ " ++ show conc
instance Eq Proof where 
    (==) :: Proof -> Proof -> Bool
    Proof terms1 prems1 conc1 == Proof terms2 prems2 conc2 = terms1 == terms2 && prems1 == prems2 && conc1 == conc2

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
    show (And a b) = show a ++ " ∧ " ++ show b
    show (Or a b) = show a ++ " ∨ " ++ show b
    show (Impl a b) = show a ++ " → " ++ show b
    show (Eq a b) = show a ++ " = " ++ show b
    show (All x a) = "∀" ++ show x ++ " " ++ show a
    show (Some x a) = "∃" ++ show x ++ " " ++ show a
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

data Predicate = Predicate String [Term]
instance Show Predicate where
    show (Predicate name []) = name
    show (Predicate name terms) = name ++ "(" ++ show terms ++ ")"
instance Eq Predicate where 
    (==) :: Predicate -> Predicate -> Bool
    Predicate a as == Predicate b bs = a == b && as == bs

data Term = Term String [Term] deriving Ord
instance Show Term where
    show (Term name []) = name
    show (Term name terms) = name ++ "(" ++ show terms ++ ")"
instance Eq Term where 
    Term a as == Term b bs = a == b && as == bs

data IDType = IDTypeTerm Integer | IDTypePred Integer