module Example (
    example
) where

import Backend.TypeChecker
import Parser.Logic.Abs
import qualified Data.Map as Map

example :: IO ()
example = do
    let env = Map.empty
        termX = Term (TermId "x") (Params [])
        termY = Term (TermId "y") (Params [])
        termA = Term (TermId "a") (Params [])
        termB = Term (TermId "b") (Params [])
        
        -- Statements
        form1 = FormEq termX termY
        form2 = FormNot (FormEq termA termB)
        
        -- Conclusion
        conclusion = FormAnd form1 form2
        
        -- Steps
        steps = [ StepPrem form1
                , StepAssume form2
                , StepForm (RuleId "someRule") [ArgLit 1] conclusion
                ]
        
        -- Sequent
        sequent = Seq [form1, form2] conclusion steps
        
        -- Check the sequent
        result = checkSequent env sequent
    
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right (checkedSequent, newEnv) -> do
            putStrLn $ "Checked Sequent: " ++ show checkedSequent
            putStrLn $ "New Environment: " ++ show newEnv
