{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Test.HUnit
import System.Directory
import System.FilePath (takeFileName, (</>), takeExtension)
import Control.Monad (unless, filterM)

import Backend.TypeChecker
import Backend.Environment
import qualified Data.List as List
import Data.Text (Text, unpack)

testGoodProof :: FilePath -> Test
testGoodProof proofPath = TestCase $ do
    case checkJson proofPath of
        Err warns env err -> 
            assertFailure $ "Wrong result: Proof is correct but reported as incorrect.\nError:\n" ++ 
                           List.intercalate "\n" [show warn | warn <- warns] ++ 
                           "\n" ++ showPos env ++ "\n" ++ show err ++ "\n"
        Ok warns _ -> do
            unless (null warns) $ 
                putStrLn $ "Warnings for " ++ proofPath ++ ":\n" ++ 
                           List.intercalate "\n" (map show warns)
            assertBool ("Proof is valid: " ++ proofPath) True

testBadProof :: FilePath -> Test
testBadProof proofPath = TestCase $ do
    case checkJson proofPath of
        Err _warns _env _err ->
            assertBool ("Proof is incorrect: " ++ proofPath) True
        Ok _warns _ -> do
            assertFailure "Wrong result: Proof is incorrect but reported as correct\n"

collectJsonFiles :: FilePath -> IO [FilePath]
collectJsonFiles dir = do
    contents <- listDirectory dir
    let fullPaths = map (dir </>) contents
    directories <- filterM doesDirectoryExist fullPaths
    files <- filterM doesFileExist fullPaths
    let jsonFiles = filter isProofFile files
    subdirsFiles <- mapM collectJsonFiles directories
    return $ jsonFiles ++ concat subdirsFiles

isProofFile :: FilePath -> Bool
isProofFile f = takeExtension f `elem` [".folke", ".json"]

testProofs :: (FilePath -> Test) -> [FilePath] -> [Test]
testProofs testFun paths = 
    [TestLabel (takeFileName path) (testFun path) | path <- paths]

testReplaceInTerm :: Term -> Term -> Term -> Term -> Test
testReplaceInTerm x t phi exp = TestCase(case replaceInTerm newEnv x t phi of
    Err _ _ err -> assertBool (show err) False
    Ok _ res -> assertEqual ("Result " ++ show res ++ " do not match expected result " ++ show exp) res exp
    )

testReplaceInTerms :: Term -> Term -> [Term] -> [Term] -> Test
testReplaceInTerms x t phi exp = TestCase(case replaceInTerms newEnv x t phi of
    Err _ _ err -> assertBool (show err) False
    Ok _ res -> assertEqual ("Result " ++ show res ++ " do not match expected result " ++ show exp) res exp
    )

testReplaceInFormula :: Term -> Term -> Formula -> Formula -> Test
testReplaceInFormula x t phi exp = TestCase(case replaceInFormula newEnv x t phi of
    Err _ _ err -> assertBool (show err) False
    Ok _ res -> assertEqual ("Result " ++ show res ++ " do not match expected result " ++ show exp) res exp
    )

testReplace :: Test
testReplace = do
    let inTermTests = TestList [
            TestLabel "Test 1" (testReplaceInTerm (Term "x" []) (Term "y" []) (Term "x" []) (Term "y" [])),
            TestLabel "Test 2" (testReplaceInTerm (Term "x_0" []) (Term "y" []) (Term "x_0" []) (Term "y" [])),
            TestLabel "Test 3" (testReplaceInTerm (Term "x" []) (Term "y_0" []) (Term "x" []) (Term "y_0" []))
            ]
    let inTermsTests = TestList [
            TestLabel "Test 1" (testReplaceInTerms (Term "x" []) (Term "y" []) [Term "x" [], Term "a" []] [Term "y" [], Term "a" []]),
            TestLabel "Test 2" (testReplaceInTerms (Term "x" []) (Term "y" []) [Term "a" [], Term "x" []] [Term "a" [], Term "y" []])
            ]
    let inFormulaTests = TestList [
            TestLabel "Test Predicate 1" (testReplaceInFormula (Term "x" []) (Term "t" []) (Pred (Predicate "P" [Term "x" [], Term "y" []])) (Pred (Predicate "P" [Term "t" [], Term "y" []]))),
            TestLabel "Test Predicate 2" (testReplaceInFormula (Term "y" []) (Term "t" []) (Pred (Predicate "P" [Term "x" [], Term "y" []])) (Pred (Predicate "P" [Term "x" [], Term "t" []]))),

            TestLabel "Test For All 1" (testReplaceInFormula -- will not replace because x is bound
                (Term "x" []) 
                (Term "t" []) 
                (All (Term "x" []) (Pred (Predicate "P" [Term "x" []]))) 
                (All (Term "x" []) (Pred (Predicate "P" [Term "x" []])))),
            TestLabel "Test For All 2" (testReplaceInFormula 
                (Term "y" []) 
                (Term "t" []) 
                (All (Term "x" []) (Pred (Predicate "P" [Term "y" []]))) 
                (All (Term "x" []) (Pred (Predicate "P" [Term "t" []])))),
            TestLabel "Test For All 3" (testReplaceInFormula -- will not replace because x is bound
                (Term "x" []) 
                (Term "t" []) 
                (All (Term "x" []) (Pred (Predicate "P" [Term "x" []]))) 
                (All (Term "x" []) (Pred (Predicate "P" [Term "x" []]))))
            ]
    TestList [ TestLabel "In Term" inTermTests, TestLabel "In Terms" inTermsTests, TestLabel "In Formula" inFormulaTests]

testUDefRule :: Env -> String -> [Formula] -> Formula -> Test
testUDefRule env name args res = TestCase(case applyRule env name [ArgForm arg | arg <- args] res of 
    Err _ _ err -> assertBool (show err) False
    Ok _ _res -> assertBool "Dummy msg" True)

testUDefRules :: Test
testUDefRules = do
    let env = addUDefRule newEnv "DMAnd" (UDefRule [Not (And (Pred (Predicate "P" [])) (Pred (Predicate "Q" [])))] (And (Not (Pred (Predicate "P" []))) (Not (Pred (Predicate "Q" [])))))
    TestList [
            TestLabel "De Morgan And 1" (testUDefRule env "DMAnd" [Not (And 
                (Pred (Predicate "P" [])) 
                (Pred (Predicate "Q" []))
            )] (And 
                (Not (Pred (Predicate "P" []))) 
                (Not (Pred (Predicate "Q" [])))
            )),
            TestLabel "De Morgan And 2" (testUDefRule env "DMAnd" [Not (And 
                (And (Pred (Predicate "A" [])) ( Pred (Predicate "B" []))) 
                (Pred (Predicate "C" []))
            )] (And 
                (Not (And (Pred (Predicate "A" [])) ( Pred (Predicate "B" [])))) 
                (Not (Pred (Predicate "C" [])))
            )),
            TestLabel "De Morgan And 3" (testUDefRule env "DMAnd" [Not (And 
                (Pred (Predicate "A" [])) 
                (And (Pred (Predicate "B" [])) ( Pred (Predicate "C" [])))
            )] (And 
                (Not (Pred (Predicate "A" []))) 
                (Not (And (Pred (Predicate "B" [])) ( Pred (Predicate "C" []))))
            ))
        ]
testParseForm :: Text -> Test
testParseForm t = TestLabel (unpack t) (TestCase (case parseForm newEnv t of
    Err _ _ err -> assertBool (show err) False
    Ok _ _ -> assertBool "Dummy message" True
    ))

testParser :: Test
testParser = TestList [
            testParseForm "A",
            testParseForm "!A",
            testParseForm "A&B",
            testParseForm "A|B",
            testParseForm "A->B",
            testParseForm "∃x A",
            testParseForm "∀x A",
            testParseForm "x=y",
            testParseForm "bot"
        ]
main :: IO ()
main = do
    -- Collect files
    exams <- collectJsonFiles "assets/examples/exams"
    book <- collectJsonFiles "assets/examples/book"
    simple <- collectJsonFiles "test/proofs/simple_tests"
    incorrect <- collectJsonFiles "test/proofs/incorrect"
    
    -- Create tests for all proof files
    let jsonFiles = exams ++ book ++ simple
    let correctTests = TestList (testProofs testGoodProof jsonFiles)
    let incorrectTests = TestList (testProofs testBadProof incorrect)
    
    -- Run all test including replace tests
    putStrLn "Running tests...\n"
    let tests = TestList [
            TestLabel "Correct proofs" correctTests,
            TestLabel "Incorrect proofs" incorrectTests,
            TestLabel "Replace" testReplace,
            TestLabel "User Defined rules" testUDefRules,            
            TestLabel "Test parser" testParser
          ]
    runTestTTAndExit tests
