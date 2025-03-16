module Main (main) where

import Test.HUnit
import System.IO
import System.Directory
import Data.Either

import Backend.TypeChecker

import Logic.Par (pSequent, myLexer)

testProof:: String -> Test
testProof proof = TestCase(do
        case checkString proof of
            Error kind msg -> assertBool (show kind ++ msg) False
            Ok seq -> assertBool "Dummy msg" True)

testProofs :: [String] -> [String]-> [Test]
testProofs [] [] = []
testProofs (name:names) (proof:proofs) = do 
    let tests = testProofs names proofs
    let test  = testProof proof
    tests ++ [TestLabel name test]


main :: IO ()
main = do
    files <- listDirectory "test/proofs"
    let paths = ["test/proofs/"++file | file <- files]
    proofs <- mapM readFile paths
    let tests = TestList (testProofs files proofs)
    runTestTTAndExit tests
