module Example (
    example
) where

import Backend.TypeChecker
import Backend.Types
import qualified Logic.Abs as Abs
import Control.Monad (unless)

example :: IO ()
example = do
    let termX = Abs.Term (Abs.Ident "x") (Abs.Params [])
        termY = Abs.Term (Abs.Ident "y") (Abs.Params [])
        termZ = Abs.Term (Abs.Ident "z") (Abs.Params [])
        termU = Abs.Term (Abs.Ident "u") (Abs.Params [])
        
        -- Statements
        form1 = Abs.FormEq termX termY
        form2 = Abs.FormEq termY termZ
        
        -- Conclusion
        conclusion = Abs.FormEq termX termZ
        
        -- Steps
        steps = [ Abs.StepPrem form1
                , Abs.StepPrem form2
                , Abs.StepForm (Abs.Ident "EqE") [Abs.ArgLine 2, Abs.ArgLine 1, Abs.ArgForm termU (Abs.FormEq termX termU)] conclusion
                ]

        proof = Abs.Proof (zipWith (\s i -> Abs.ProofElem [Abs.LabelLine i] s) steps [1..])
        
        -- Sequent
        sequent = Abs.Seq [form1, form2] conclusion proof
        
        -- Check the sequent
        result = check sequent
    
    case result of
        Err warns _ err -> do
            putStrLn $ "Error: " ++ show err
            unless (null warns) $ do
                putStrLn "Warnings:"
                mapM_ (putStrLn . ("  " ++) . show) warns
        Ok warns _ -> do
            putStrLn "Proof is correct!"
            unless (null warns) $ do
                putStrLn "Warnings:"
                mapM_ (putStrLn . ("  " ++) . show) warns