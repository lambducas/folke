module Main (main) where

import Test.HUnit
import System.IO
import System.Directory
import Data.Either

import Backend.TypeChecker
import Backend.Types

import Logic.Par (pSequent, myLexer)

testProofGood:: String -> Test
testProofGood proof = TestCase(do
        case checkString proof of
            Error _ kind msg -> assertBool (show kind ++ ": " ++ msg) False
            Ok _ seq -> assertBool "Dummy msg" True)

testProofBadType:: String -> Test
testProofBadType proof = TestCase(do
        case checkString proof of
            Error _ TypeError _ ->  assertBool "Dummy msg" True
            Error _ kind msg -> case kind of
                TypeError -> assertBool "Dummy msg" True
                _ -> assertBool ("Expected TypeError not " ++ show kind ++ ": " ++ msg) False
            Ok _ _ -> assertBool "Did not fail as expected" False)

testProofs :: (String -> Test) -> [String] -> [String]-> [Test]
testProofs test_fun [] [] = []
testProofs test_fun (name:names) (proof:proofs) = do 
    let tests = testProofs test_fun names proofs
    let test  = test_fun proof
    tests ++ [TestLabel name test]


main :: IO ()
main = do
    files <- listDirectory "test/proofs/good"
    let paths = ["test/proofs/good/"++file | file <- files]
    proofs <- mapM readFile paths
    let tests_good = TestList (testProofs testProofGood files proofs)

    files <- listDirectory "test/proofs/bad_type/"
    let paths = ["test/proofs/bad_type/"++file | file <- files]
    proofs <- mapM readFile paths
    let tests_bad_type = TestList (testProofs testProofBadType files proofs)
    let tests = TestList [TestLabel "Good" tests_good, TestLabel "Bad Types"  tests_bad_type]
    runTestTTAndExit tests
