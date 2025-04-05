module Main (main) where

import Test.HUnit
import System.Directory

import Backend.TypeChecker
import Backend.Types
import Backend.Environment

import Logic.Par (pSequent, myLexer)

testProofGood:: String -> Test
testProofGood proof = TestCase(do
        case checkString proof of
            Error _ env err -> assertBool (showPos env ++ show err) False
            Ok _ _ -> assertBool "Dummy msg" True)

testProofBadType:: String -> Test
testProofBadType proof = TestCase(do
        case checkString proof of
            Error _ _ err@(SyntaxError _) ->  assertBool (show err) True
            Error _ _ _ -> assertBool "Dummy msg" True
            Ok _ _ -> assertBool "Did not fail as expected" False)

testProofs :: (String -> Test) -> [String] -> [String]-> [Test]
testProofs _ [] [] = []
testProofs _ _ [] = error "fewer proofs then names"
testProofs _ [] _ = error "fewer names then proofs"
testProofs test_fun (name:names) (proof:proofs) = do 
    let tests = testProofs test_fun names proofs
    let test  = test_fun proof
    tests ++ [TestLabel name test]

testReplaceInTerm :: Term -> Term -> Term -> Term -> Test
testReplaceInTerm x t phi exp =  TestCase(case replaceInTerm newEnv x t phi of
    Error _ _ err -> assertBool (show err) False
    Ok _ res -> assertEqual ("Result " ++ show res ++ " do not match expected result " ++ show exp) res exp
    )

testReplaceInTerms :: Term -> Term -> [Term] -> [Term] -> Test
testReplaceInTerms x t phi exp = TestCase(case replaceInTerms newEnv x t phi of
    Error _ _ err -> assertBool (show err) False
    Ok _ res -> assertEqual ("Result " ++ show res ++ " do not match expected result " ++ show exp) res exp
    )

testReplaceInFormula :: Term -> Term -> Formula -> Formula -> Test
testReplaceInFormula x t phi exp = TestCase(case replaceInFormula newEnv x t phi of
    Error _ _ err -> assertBool (show err) False
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

            TestLabel "Test For All 1" (testReplaceInFormula 
                (Term "x" []) 
                (Term "t" []) 
                (All (Term "x" []) (Pred (Predicate "P" [Term "x" []]))) 
                (All (Term "t" []) (Pred (Predicate "P" [Term "t" []])))),
            TestLabel "Test For All 2" (testReplaceInFormula 
                (Term "y" []) 
                (Term "t" []) 
                (All (Term "x" []) (Pred (Predicate "P" [Term "y" []]))) 
                (All (Term "x" []) (Pred (Predicate "P" [Term "t" []])))),
            TestLabel "Test For All 3" (testReplaceInFormula 
                (Term "x" []) 
                (Term "t" []) 
                (All (Term "x" []) (Pred (Predicate "P" [Term "x" []]))) 
                (All (Term "t" []) (Pred (Predicate "P" [Term "t" []]))))
            ]
    TestList [ TestLabel "In Term" inTermTests, TestLabel "In Terms" inTermsTests, TestLabel "In Formula" inFormulaTests]
main :: IO ()
main = do
    good_files <- listDirectory "test/proofs/good"
    let good_paths = ["test/proofs/good/"++file | file <- good_files]
    good_proofs <- mapM readFile good_paths
    let tests_good = TestList (testProofs testProofGood good_files good_proofs)

    bad_files <- listDirectory "test/proofs/bad_type/"
    let bad_paths = ["test/proofs/bad_type/"++file | file <- bad_files]
    bad_proofs <- mapM readFile bad_paths
    let tests_bad_type = TestList (testProofs testProofBadType bad_files bad_proofs)
    let tests = TestList [TestLabel "Good" tests_good, TestLabel "Bad Types"  tests_bad_type, TestLabel "Replace" testReplace]
    runTestTTAndExit tests
