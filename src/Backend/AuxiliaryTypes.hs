Module AuxiliaryTypes where

-- | Types for typechecker
newtype TypeException e m a = TypeException 
  { 
    runException :: m (Either e a)
  }

newtype Err = Err String

type Result a = TypeException Err (State Env) a

-- | IDEA
-- 
--   For all possible datatypes
--   check well-formedness with this type:
checkTYPE :: Parser.Abs.TYPE -> Result ()

-- | Make Result a monad so we can chain like this:
--   Example with FORM

checkForm :: Parser.Abs.Form-> Result ()
checkForm f -> \case
  (Form t1 t2) -> checkTerm t1 >> checkTerm t2
  -- .........
  -- .........

checkTerm :: Parser.Abs.Term -> Result a
checkTerm = Result $ error "Not implemented"

-- | För error-handling à Left err
-- och sequencing á (>>=) 
-- så måste TypeException vara Monad
-- så vi implementerar det


instance Applicative (TypeException e a) where
instance Functor ... where -- implement

instance Monad (TypeException e a) where
    return x = TypeException $ Right x
     
    (TypeException e a) >>= f = undefined