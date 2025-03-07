module Example (
    example
) where

example :: IO ()
example = do
    let env = Map.empty
        termX = Term (TermId "x") (Params [])
        termY = Term (TermId "y") (Params [])
        termZ = Term (TermId "z") (Params [])
        
        -- Statements
        form1 = FormEq termX termY
        form2 = FormEq termY termZ
        form3 = FormEq termX termZ
        
        -- Conclusion
        conclusion = form3
        
        -- Steps
        steps = [ StepPrem form1
                , StepPrem form2
                , StepForm (RuleId "equalityElim") [ArgLit 1, ArgLit 2] conclusion
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
            if isProofCorrect checkedSequent
                then putStrLn "Proof is correct!"
                else putStrLn "Proof is incorrect!"
