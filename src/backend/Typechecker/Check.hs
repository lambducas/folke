module Typechecker.Check where


{-
Seq [FormAll (VarLit (VarId "x")) FormBot] (FormSome (VarLit (VarId "y")) 
FormBot) [StepPrem (FormAll (VarLit (VarId "x")) FormBot),StepPrem 
(FormSome (VarLit (VarId "y")) 
FormBot),StepScope [StepFree (VarLit (VarId "x"))]] -}

check :: IO()
check = undefined