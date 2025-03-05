module Example where

import Backend.TypeChecker
import Parser.Logic.Abs
import qualified Data.Map as Map

example :: IO ()
example = do
    let env = Map.empty
        term1 = Term (TermId "x") (Params [])
        term2 = Term (TermId "y") (Params [])
        form = FormEq term1 term2
        result = checkForm env form
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right (checkedForm, newEnv) -> do
            putStrLn $ "Checked Form: " ++ show checkedForm
            putStrLn $ "New Environment: " ++ show newEnv
