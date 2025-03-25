module Frontend.Helper (
  evalPath,
  maybeHead,
  getProofFileByPath,
  removeIdx,
  slice,
  trim,
  trimText,
  firstKeystroke,
  fontListToText
) where

import Frontend.Types
import Monomer
import Data.Char (isSpace)
import Data.Text (Text, pack, unpack)
import Data.List (find, dropWhileEnd)

-- -- Placeholder
-- parseProofToFile :: FESequent -> Text
-- parseProofToFile sequent = "\n" <> premises <> ";\n" <> conclusion <> ";\n" <> exportProofHelper (SubProof (_steps sequent)) 0
--   where
--     premises = replaceSpecialSymbolsInverse $ intercalate "," (_premises sequent)
--     conclusion = replaceSpecialSymbolsInverse $ _conclusion sequent

--     exportProofHelper :: FEStep -> Int -> Text
--     exportProofHelper (SubProof p) indent = tabs indent <> "{\n" <> intercalate "\n" (map (`exportProofHelper` (indent + 1)) p) <> "\n" <> tabs indent <> "}"
--     exportProofHelper (Line statement rule) indent = tabs indent <> statement <> " : " <> rule <> ";"

--     tabs :: Int -> Text
--     tabs n = pack $ replicate n '\t'

-- -- Placeholder
-- parseProofFromFile :: Text -> FESequent
-- parseProofFromFile p = case proof of
--   [SubProof p] -> FESequent premises conclusion p
--   _ -> error "Corrupt proof from `parseText`"
--   where
--     premises = filter (/="") (map trimText (splitOn "," (pack $ slice 0 premiseEnd text)))
--     conclusion = trimText (pack (slice (premiseEnd + 1) conclusionEnd text))

--     premiseEnd = snd $ fromMaybe (error "Empty premise") (gotoNextChar text (-1) [';'])
--     conclusionEnd = snd $ fromMaybe (error "Empty conclusion") (gotoNextChar text premiseEnd [';'])
--     proofStart = snd $ fromMaybe (error "No proof") (gotoNextChar text conclusionEnd ['{'])

--     proof = [parseText text proofStart []]
--     -- proof = [parseText (trim $ drop (conclusionEnd + 1) text) 0 []]
--     -- proof = [parseText text 0 []]
--     text = unpack p

--     parseText :: String -> Int -> [FEStep] -> FEStep
--     parseText text ptr formulas = case nextSpecialChar of
--       Just (char, idx) -> case char of
--         ';' -> parseText text idx (formulas ++ [parseFormula $ pack $ slice (ptr + 1) idx text])
--         '{' -> parseText text (findClosingBracket text 0 idx 0 + 1) (formulas ++ [parseText (slice (idx + 1) (findClosingBracket text 0 idx 0 + 1) text) 0 []])
--         '}' -> SubProof formulas
--         _ -> error "Invalid special char"
--       Nothing -> SubProof [Line "" ""] -- Return garbage instead of crashing
--       where
--         nextSpecialChar = gotoNextChar text ptr ['{', '}', ';']

--     parseFormula :: Text -> FEStep
--     parseFormula text = Line statement rule
--       where
--         statement = trimText $ head parts
--         rule = trimText $ parts !! 1
--         parts = splitOn ":" text

--     gotoNextChar text ptr chars
--       | ptr >= len - 1 = Nothing
--       | currentChar `elem` chars = Just (currentChar, ptr + 1)
--       | otherwise = gotoNextChar text (ptr + 1) chars
--       where
--         len = length text
--         currentChar = text !! (ptr + 1)

--     findClosingBracket :: [Char] -> Int -> Int -> Int -> Int
--     findClosingBracket text nestedLevel idx cnl
--       | idx >= length text - 1 = idx -- error "No closing bracket found"
--       | otherwise = case char of
--       '{' -> findClosingBracket text nestedLevel (idx + 1) (cnl + 1)
--       '}' -> if nestedLevel == cnl then idx + 1 else findClosingBracket text nestedLevel (idx + 1) (cnl - 1)
--       _ -> findClosingBracket text nestedLevel (idx + 1) cnl
--       where char = text !! (idx + 1)

getProofFileByPath :: [File] -> FilePath -> Maybe File
getProofFileByPath allFiles filePath = find (\f -> _path f == filePath) allFiles

evalPath :: FESequent -> FormulaPath -> FEStep
evalPath sequent formulaPath = ep formulaPath (SubProof $ _steps sequent)
  where
    ep (idx:rest) currentProof = case currentProof of
      SubProof p -> ep rest $ p !! idx
      Line _ _ -> error "Tried to index into `Line` (not an array)"
    ep [] p = p

firstKeystroke :: [(Text, AppEvent, Bool)] -> WidgetNode s AppEvent -> WidgetNode s AppEvent
firstKeystroke ((key, event, enabled):xs) widget = keystroke_ [(key, if enabled then event else NoEvent)] [ignoreChildrenEvts] (firstKeystroke xs widget)
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