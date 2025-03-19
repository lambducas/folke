module Backend.Types (
    Sequent(Sequent),
    Formula(Pred, And, Nil),
    Predicate(Predicate),
    Result(Ok, Error),
    ErrorKind(TypeError,SyntaxError, UnknownError)
) where
data Result a = Ok a | Error ErrorKind String

data Sequent = Sequent [Formula] Formula
instance Show Sequent where
    show (Sequent prems conc) = show prems ++ "|-" ++ show conc
instance Eq Sequent where 
    Sequent prems1 conc1 == Sequent prems2 conc2 = prems1 == prems2 && conc1 == conc2

data Formula = 
            Pred Predicate |
            And Formula Formula |
            Nil
instance Show Formula where
    show (Pred a) = show a
    show (And a b) = show a ++ "&" ++ show b
    show Nil = "Nil"
instance Eq Formula where 
    And a1 a2 == And b1 b2 = a1 == b1 && a2 == b2
    Pred a == Pred b = a==b
    Nil == Nil = True
    _ == _ = False


data Predicate = Predicate String 
instance Show Predicate where
    show (Predicate name) = name
instance Eq Predicate where 
    Predicate a == Predicate b = a == b

data ErrorKind = TypeError | SyntaxError | UnknownError deriving Show 