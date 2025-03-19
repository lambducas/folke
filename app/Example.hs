module Example (
    example
) where

import Backend.TypeChecker
import Logic.Abs
import Backend.Types

example :: IO ()
example = do
    let termX = Term (Ident "x") (Params [])
        termY = Term (Ident "y") (Params [])
        termZ = Term (Ident "z") (Params [])
        
        -- Statements
        form1 = FormEq termX termY
        form2 = FormEq termY termZ
        form3 = FormEq termX termZ
        
        -- Conclusion
        conclusion = form3
        
        -- Steps
        steps = [ StepPrem form1
                , StepPrem form2
                , StepForm (Ident "equalityElim") [ArgLit 1, ArgLit 2] conclusion
                ]

        proof = Proof (map (ProofElem []) steps)
        
        -- Sequent
        sequent = Seq [form1, form2] conclusion proof
        
        -- Check the sequent
        result = check sequent
    
    case result of
        Error _kind err -> putStrLn $ "Error: " ++ show err
        Ok _seq -> do
            putStrLn "Proof is correct!"