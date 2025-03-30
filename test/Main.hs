module Main (main) where

import Test.HUnit
import System.Directory

import Backend.TypeChecker
import Backend.Types

import Logic.Par (pSequent, myLexer)

testProofGood:: String -> Test
testProofGood proof = TestCase(do
        case checkString proof of
            Error _ err -> assertBool (show err) False
            Ok _ _ -> assertBool "Dummy msg" True)

testProofBadType:: String -> Test
testProofBadType proof = TestCase(do
        case checkString proof of
            Error _ err@(SyntaxError _) ->  assertBool (show err) True
            Error _ _ -> assertBool "Dummy msg" True
            Ok _ _ -> assertBool "Did not fail as expected" False)

testProofs :: (String -> Test) -> [String] -> [String]-> [Test]
testProofs _ [] [] = []
testProofs _ _ [] = error "fewer proofs then names"
testProofs _ [] _ = error "fewer names then proofs"
testProofs test_fun (name:names) (proof:proofs) = do 
    let tests = testProofs test_fun names proofs
    let test  = test_fun proof
    tests ++ [TestLabel name test]

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
    let tests = TestList [TestLabel "Good" tests_good, TestLabel "Bad Types"  tests_bad_type]
    runTestTTAndExit tests
