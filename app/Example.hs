module Example (
    example
) where

import Backend.TypeChecker
import Backend.Types
import qualified Logic.Abs as Abs

example :: IO ()
example = do
    let termX = Abs.Term (Abs.Ident "x") (Abs.Params [])
        termY = Abs.Term (Abs.Ident "y") (Abs.Params [])
        termZ = Abs.Term (Abs.Ident "z") (Abs.Params [])
        
        -- Statements
        form1 = Abs.FormEq termX termY
        form2 = Abs.FormEq termY termZ
        form3 = Abs.FormEq termX termZ
        
        -- Conclusion
        conclusion = form3
        
        -- Steps
        steps = [ Abs.StepPrem form1
                , Abs.StepPrem form2
                , Abs.StepForm (Abs.Ident "equalityElim") [Abs.ArgLine 1, Abs.ArgLine 2] conclusion
                ]

        proof = Abs.Proof (map (Abs.ProofElem []) steps)
        
        -- Sequent
        sequent = Abs.Seq [form1, form2] conclusion proof
        
        -- Check the sequent
        result = check sequent
    
    case result of
        Error _kind err -> putStrLn $ "Error: " ++ show err
        Ok _seq -> do
            putStrLn "Proof is correct!"