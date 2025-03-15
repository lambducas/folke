module Main (main) where

import Test.HUnit
import System.IO
import System.Directory
import Data.Either

import Backend.TypeChecker

import Logic.Par (pSequent, myLexer)

testProof:: String -> Test
testProof proof = TestCase(do
        case pSequent (myLexer proof) of
            Left err -> assertBool ("SYNTAX ERROR: " ++ err) False
            Right seq -> do
                let result = check seq
                let error  = fromLeft (Unknown "This is not an error") result
                assertBool ("TYPE ERROR: " ++ show error) (isRight result))

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
