{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.Helper (
  isFileEdited,
  evalPath,
  pathToLineNumber,
  maybeHead,
  getProofFileByPath,
  removeIdx,
  slice,
  trim,
  trimText,
  trimExtension,
  firstKeystroke,
  fontListToText,
  parseProofForBackend
) where

import Frontend.SpecialCharacters
import Frontend.Types
import Monomer
import Data.Char (isSpace)
import Data.Text (Text, pack, unpack, intercalate, splitOn)
import Data.List (find, dropWhileEnd)
import TextShow (showt)

isFileEdited :: Maybe File -> Bool
isFileEdited (Just f@ProofFile {}) = _isEdited f
isFileEdited Nothing = False
isFileEdited _ = False

parseProofForBackend :: FESequent -> Text
parseProofForBackend sequent = premises <> " |- " <> conclusion <> " " <> exportProofHelper 0 [] proof
  where
    premises = replaceSpecialSymbolsInverse $ intercalate "," (_premises sequent)
    conclusion = replaceSpecialSymbolsInverse $ _conclusion sequent

    newSequent = FESequent (_premises sequent) (_conclusion sequent) (ghostPremises ++ _steps sequent)
    proof = SubProof (_steps newSequent)
    ghostPremises = map (\p -> Line p "prem") (_premises sequent)

    exportProofHelper :: Int -> FormulaPath -> FEStep -> Text
    exportProofHelper indent path (SubProof p) = tabs indent <> label <> "{\n" <> intercalate "\n" (zipWith (\p idx -> exportProofHelper (indent + 1) (path ++ [idx]) p) p [0..]) <> "\n" <> tabs indent <> "}"
      where label = if null p || null path then "" else showt (offsetLineNumber (path ++ [0])) <> "-" <> showt (offsetLineNumber (path ++ [length p - 1])) <> ":"
    exportProofHelper indent path (Line statement rule) = tabs indent <> label <> nRule <> " " <> nStatement <> ";"
      where
        nRule = replaceSpecialSymbolsInverse rule
        nStatement = replaceSpecialSymbolsInverse statement
        label = showt (offsetLineNumber path) <> ":"

    offsetLineNumber path = pathToLineNumber newSequent path-- + toInteger (length (_premises sequent))

    tabs :: Int -> Text
    tabs n = pack $ replicate n '\t'

getProofFileByPath :: [File] -> FilePath -> Maybe File
getProofFileByPath allFiles filePath = find (\f -> _path f == filePath) allFiles

evalPath :: FESequent -> FormulaPath -> FEStep
evalPath sequent formulaPath = ep formulaPath (SubProof $ _steps sequent)
  where
    ep (idx:rest) currentProof = case currentProof of
      SubProof p -> ep rest $ p !! idx
      Line _ _ -> error "Tried to index into `Line` (not an array)"
    ep [] p = p

pathToLineNumber :: FESequent -> FormulaPath -> Integer
pathToLineNumber sequent path = ep path (SubProof $ _steps sequent) 1
  where
    ep (idx:tail) currentProof startLine = case currentProof of
      SubProof p -> ep tail (p !! idx) (startLine + sum (map stepLength (take idx p)))
      Line _ _ -> error "Tried to index into `Line` (not an array)"
    ep [] _ startLine = startLine

    stepLength (SubProof p) = sum $ map stepLength p
    stepLength (Line _ _) = 1

firstKeystroke :: [(Text, AppEvent, Bool)] -> WidgetNode s AppEvent -> WidgetNode s AppEvent
firstKeystroke ((key, event, enabled):xs) widget = keystroke_ [(key, if enabled then event else NoEvent)] [ignoreChildrenEvts | enabled] (firstKeystroke xs widget)
firstKeystroke [] widget = widget

fontListToText :: [String] -> Text
fontListToText fontList | head fontList == "Regular" = "Default"
                        | head fontList == "Roboto_Regular" = "Roboto"
                        | head fontList == "Comic_Sans_Thin" = "Comic Sans"
                        | head fontList == "Dyslexic" = "Dyslexic"
                        | otherwise = "forgor_to_label"

-- https://www.youtube.com/watch?v=aS8O-F0ICxw
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (h:_) = Just h

removeIdx :: Int -> [a] -> [a]
removeIdx idx lst = part1 ++ drop 1 part2 where
  (part1, part2) = splitAt idx lst

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)

trim :: [Char] -> [Char]
trim = dropWhileEnd isSpace . dropWhile isSpace

trimText :: Text -> Text
trimText = pack . trim . unpack

trimExtension :: Text -> Text -> Text
trimExtension ext text
  | hasExt = intercalate ext (init split)
  | otherwise = text
  where
    hasExt = length split >= 2 && last split == ""
    split = splitOn ext (trimText text)