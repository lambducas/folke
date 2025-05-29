{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.Parse (
  parseProofToJSON,
  parseProofFromJSON,
  parseProofToSimpleFileFormat,
  parseProofFromSimpleFileFormat,
  parseProofForBackend,
  validateStatement,
  validateRule,
  validateRuleArgument,
  parseRule
) where

import Frontend.Types ( FEStep(SubProof, Line), FESequent(..), FormulaPath, ruleMetaDataMap, visualRuleNames, FEUserDefinedRule (_udrName) )
import Frontend.Helper.General ( slice, trimText, pathToLineNumber )
import Logic.Par (myLexer, pForm, pArg)
import Shared.SpecialCharacters ( replaceSpecialSymbolsInverse, replaceSpecialSymbols, replaceFromInverseLookup )
import Shared.FESequent (FEDocument(..))

import Data.Aeson ( decode )
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Either (isRight)
import Data.Maybe (isJust)
import Data.Text.Internal.Builder (toLazyText)
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder(toLazyByteString)
import qualified Data.Text as T
import qualified Data.Map
import qualified Data.List
import TextShow (showt)

-- | Check if the provided string is a valid way to write a formula
validateStatement :: T.Text -> Bool
validateStatement statement = isRight res
  where
    res = pForm (myLexer s)
    s = T.unpack (replaceSpecialSymbolsInverse statement)

-- | Check if the provided string is a valid way to write a rule
validateRule :: FEDocument -> T.Text -> Bool
validateRule document rule = parsedRule == "" || isJust (Data.Map.lookup parsedRule ruleMetaDataMap) || isCustomRule
  where
    parsedRule = parseRule rule

    isCustomRule = case _fedUserDefinedRules document of
      Nothing -> False
      Just rules -> isJust $ Data.List.find (\r -> _udrName r == rule) rules

-- | Check if the provided string is a valid way to write a rule argument
validateRuleArgument :: T.Text -> Bool
validateRuleArgument arg = arg == "" || isRight res
  where
    res = pArg (myLexer s)
    s = T.unpack (replaceSpecialSymbolsInverse arg)

-- | Converts all valid ways to write a rule to a single rule identifier
parseRule :: T.Text -> T.Text
parseRule inp = replaceFromInverseLookup withSpec visualRuleNames
  where withSpec = replaceSpecialSymbols inp

-- | Converts frontend sequent to a json string
parseProofToJSON :: FEDocument -> T.Text
parseProofToJSON = toStrict . toLazyText . encodePrettyToTextBuilder

-- | Converts a json string to a frontend sequent
parseProofFromJSON :: T.Text -> Maybe FEDocument
parseProofFromJSON = decode . toLazyByteString . encodeUtf8Builder

-- | Deprecated: Converts frontend sequent to a simple file format which is easy to parse by hand
parseProofToSimpleFileFormat :: FESequent -> T.Text
parseProofToSimpleFileFormat sequent = "\n" <> premises <> ";\n" <> conclusion <> ";\n" <> exportProofHelper (SubProof (_steps sequent)) 0
  where
    premises = replaceSpecialSymbolsInverse $ T.intercalate "," (_premises sequent)
    conclusion = replaceSpecialSymbolsInverse $ _conclusion sequent

    exportProofHelper :: FEStep -> Int -> T.Text
    exportProofHelper (SubProof p) indent = tabs indent <> "{\n" <> T.intercalate "\n" (map (`exportProofHelper` (indent + 1)) p) <> "\n" <> tabs indent <> "}"
    exportProofHelper (Line statement rule usedArguments arguments) indent = tabs indent <> statement <> " : " <> rule <> " " <> nArg rule usedArguments arguments <> ";"

    tabs :: Int -> T.Text
    tabs n = T.pack $ replicate n '\t'

-- | Deprecated: Converts a string-representation of a frontend sequent in a simple format to a FESequent
parseProofFromSimpleFileFormat :: T.Text -> Maybe FESequent
parseProofFromSimpleFileFormat p = case proof of
  Just [SubProof p] -> case premises of
    Nothing -> Nothing
    Just premises -> case conclusion of
      Nothing -> Nothing
      Just conclusion -> Just (FESequent premises conclusion p)
  _ -> Nothing--error "Corrupt proof from `parseText`"
  where
    premises = premiseEnd >>= (\pe -> Just $ filter (/="") (map trimText (T.splitOn "," (T.pack $ slice 0 pe text))))
    conclusion = case premiseEnd of
      Nothing -> Nothing
      Just premiseEnd -> case conclusionEnd of
        Nothing -> Nothing
        Just conclusionEnd -> Just $ trimText (T.pack (slice (premiseEnd + 1) conclusionEnd text))

    premiseEnd = gotoNextChar text (-1) [';'] >>= Just . snd
    conclusionEnd = (premiseEnd >>= (\f -> gotoNextChar text f [';'])) >>= Just . snd
    proofStart = (conclusionEnd >>= (\f -> gotoNextChar text f ['{'])) >>= Just . snd

    -- premiseEnd = snd $ fromMaybe (throw NoPrem) (gotoNextChar text (-1) [';'])
    -- conclusionEnd = snd $ fromMaybe (throw NoConc) (gotoNextChar text premiseEnd [';'])
    -- proofStart = snd $ fromMaybe (throw NoProof) (gotoNextChar text conclusionEnd ['{'])

    proof = proofStart >>= (\f -> Just [parseText text f []])
    -- proof = [parseText (trim $ drop (conclusionEnd + 1) text) 0 []]
    -- proof = [parseText text 0 []]
    text = T.unpack p

    parseText :: String -> Int -> [FEStep] -> FEStep
    parseText text ptr formulas = case nextSpecialChar of
      Just (char, idx) -> case char of
        ';' -> parseText text idx (formulas ++ [parseFormula $ T.pack $ slice (ptr + 1) idx text])
        '{' -> parseText text (findClosingBracket text 0 idx 0 + 1) (formulas ++ [parseText (slice (idx + 1) (findClosingBracket text 0 idx 0 + 1) text) 0 []])
        '}' -> SubProof formulas
        _ -> error "Invalid special char"
      Nothing -> SubProof [Line "" "" 0 []] -- Return garbage instead of crashing
      where
        nextSpecialChar = gotoNextChar text ptr ['{', '}', ';']

    parseFormula :: T.Text -> FEStep
    parseFormula text = Line statement rule usedArguments arguments
      where
        statement = trimText $ head parts
        rule = trimText $ parts !! 1
        usedArguments = 0
        arguments = []
        parts = T.splitOn ":" text

    gotoNextChar text ptr chars
      | ptr >= len - 1 = Nothing
      | currentChar `elem` chars = Just (currentChar, ptr + 1)
      | otherwise = gotoNextChar text (ptr + 1) chars
      where
        len = length text
        currentChar = text !! (ptr + 1)

    findClosingBracket :: [Char] -> Int -> Int -> Int -> Int
    findClosingBracket text nestedLevel idx cnl
      | idx >= length text - 1 = idx -- error "No closing bracket found"
      | otherwise = case char of
      '{' -> findClosingBracket text nestedLevel (idx + 1) (cnl + 1)
      '}' -> if nestedLevel == cnl then idx + 1 else findClosingBracket text nestedLevel (idx + 1) (cnl - 1)
      _ -> findClosingBracket text nestedLevel (idx + 1) cnl
      where char = text !! (idx + 1)

-- | Deprecated: Convert frontend sequent to string-representation used by backend (.logic format)
parseProofForBackend :: FESequent -> T.Text
parseProofForBackend sequent = premises <> " |- " <> conclusion <> " " <> exportProofHelper 0 [] proof
  where
    premises = replaceSpecialSymbolsInverse $ T.intercalate "," (_premises sequent)
    conclusion = replaceSpecialSymbolsInverse $ _conclusion sequent

    newSequent = FESequent (_premises sequent) (_conclusion sequent) (ghostPremises ++ _steps sequent)
    proof = SubProof (_steps newSequent)
    ghostPremises = map (\p -> Line p "prem" 0 []) (_premises sequent)

    exportProofHelper :: Int -> FormulaPath -> FEStep -> T.Text
    exportProofHelper indent path (SubProof p) = tabs indent <> label <> "{\n" <> T.intercalate "\n" (zipWith (\p idx -> exportProofHelper (indent + 1) (path ++ [idx]) p) p [0..]) <> "\n" <> tabs indent <> "}"
      where label = if null p || null path then "" else showt (offsetLineNumber (path ++ [0])) <> "-" <> showt (offsetLineNumber (path ++ [length p - 1])) <> ":"
    exportProofHelper indent path (Line statement rule usedArguments arguments) = tabs indent <> label <> nRule <> "" <> nArg rule usedArguments arguments <> " " <> nStatement <> ";"
      where
        nRule = parseRule rule
        nStatement = replaceSpecialSymbolsInverse statement
        label = showt (offsetLineNumber path) <> ":"

    offsetLineNumber = pathToLineNumber newSequent-- + toInteger (length (_premises sequent))

    tabs :: Int -> T.Text
    tabs n = T.pack $ replicate n '\t'

{-|
Some rules omit the brackets when no arguments are passed. This function
generates a correct string representation of the argument list
-}
nArg :: T.Text -> Int -> [T.Text] -> T.Text
nArg "fresh" _ [] = ""
nArg "prem" _ [] = ""
nArg "assume" _ [] = ""
nArg "" _ [] = ""
nArg _ usedArguments arguments = "[" <> T.intercalate ", " (map replaceSpecialSymbolsInverse (take usedArguments arguments)) <> "]"