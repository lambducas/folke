{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.Parse (
  parseProofToJSON,
  parseProofFromJSON,
  parseProofToSimpleFileFormat,
  parseProofFromSimpleFileFormat
) where

import Frontend.Types ( FEStep(SubProof, Line), FESequent(..) )
import Frontend.SpecialCharacters ( replaceSpecialSymbolsInverse )
import Frontend.Helper ( slice, trimText )

import Data.Aeson ( encode, decode, defaultOptions )
import Data.Aeson.TH ( deriveJSON )
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL

data FEDocument = FEDocument {
  _sequent :: FESequent
} deriving (Show, Eq)

$(deriveJSON defaultOptions ''FEStep)
$(deriveJSON defaultOptions ''FESequent)
$(deriveJSON defaultOptions ''FEDocument)

parseProofToJSON :: FESequent -> T.Text
parseProofToJSON = T.pack . BL.unpack . encode . FEDocument

parseProofFromJSON :: T.Text -> Maybe FESequent
parseProofFromJSON t = (decode . BL.pack . T.unpack) t >>= Just . _sequent

parseProofToSimpleFileFormat :: FESequent -> T.Text
parseProofToSimpleFileFormat sequent = "\n" <> premises <> ";\n" <> conclusion <> ";\n" <> exportProofHelper (SubProof (_steps sequent)) 0
  where
    premises = replaceSpecialSymbolsInverse $ T.intercalate "," (_premises sequent)
    conclusion = replaceSpecialSymbolsInverse $ _conclusion sequent

    exportProofHelper :: FEStep -> Int -> T.Text
    exportProofHelper (SubProof p) indent = tabs indent <> "{\n" <> T.intercalate "\n" (map (`exportProofHelper` (indent + 1)) p) <> "\n" <> tabs indent <> "}"
    exportProofHelper (Line statement rule) indent = tabs indent <> statement <> " : " <> rule <> ";"

    tabs :: Int -> T.Text
    tabs n = T.pack $ replicate n '\t'

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
      Nothing -> SubProof [Line "" ""] -- Return garbage instead of crashing
      where
        nextSpecialChar = gotoNextChar text ptr ['{', '}', ';']

    parseFormula :: T.Text -> FEStep
    parseFormula text = Line statement rule
      where
        statement = trimText $ head parts
        rule = trimText $ parts !! 1
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