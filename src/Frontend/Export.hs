{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.Export (
  convertToLatex
) where

import Data.Text (Text)
import qualified Data.Text as T
import Frontend.Types
import Frontend.SpecialCharacters (replaceSpecialSymbols)
import Frontend.Helper (getProofFileByPath)
import Control.Lens ((^.))

-- | Convert the proof model to LaTeX format
convertToLatex :: AppModel -> Text
convertToLatex model = T.unlines
  [ "\\documentclass{article}"
  , "\\usepackage{amsmath}"
  , "\\usepackage{amssymb}" -- For \ulcorner symbol
  , "\\usepackage{logicproof}"
  , ""
  , "\\begin{document}"
  , ""
  , "\\begin{center}"
  , "\\section*{" <> sectionTitle <> "}"
  , "\\end{center}"
  , ""
  -- Extract the proof from the model and format it
  , formatProof model
  , ""
  , "\\end{document}"
  ]
  where
    -- Get the filename from the current file path for the section title
    sectionTitle = case model ^. preferences . currentFile of
      Nothing -> "Formal Proof"
      Just _filePath -> "Some submission" -- T.pack $ takeBaseName filePath

-- Format the proof as LaTeX
formatProof :: AppModel -> Text
formatProof model = maybe "% No proof available" formatSequent currentSeq
  where
    currentSeq = do
      filePath <- model ^. preferences . currentFile
      file <- getProofFileByPath (model ^. preferences . tmpLoadedFiles) filePath
      case file of
        ProofFile {_parsedSequent = Just s} -> Just s
        TemporaryProofFile {_parsedSequent = Just s} -> Just s
        _ -> Nothing

formatSequent :: FESequent -> Text
formatSequent seq = T.unlines
  [ "\\subsection*{Sequent}"
  , "\\begin{center}"
  , "$" <> formattedSequent <> "$"
  , "\\end{center}"
  , ""
  , "\\subsection*{Proof}"
  , "\\begin{logicproof}{" <> (T.pack . show) maxNestedLevel <> "}"
  , premiseLines
  , formatSteps (_steps seq)
  , "\\end{logicproof}"
  ]
  where
    maxNestedLevel = subProofDepth seq
    formattedSequent =
      if null (_premises seq)
        then "\\vdash " <> formatFormula (_conclusion seq)
        else T.intercalate ", " (map formatFormula (_premises seq)) <>
             " \\vdash " <> formatFormula (_conclusion seq)

    -- Add the premises as initial lines in the proof
    premiseLines = if null (_premises seq)
                   then ""
                   else T.intercalate "\n" (map premiseLine (_premises seq))

    premiseLine premise = formatFormula premise <> " & premise \\\\"

    formatFormula = T.pack . replaceLatexSymbols . T.unpack . replaceSpecialSymbols

-- Format steps with proper enumeration
formatSteps :: [FEStep] -> Text
formatSteps steps = T.intercalate "\n" (zipWith (\s i -> formatStep s (i == length steps)) steps [1..])

-- Format a single step
formatStep :: FEStep -> Bool -> Text
formatStep (Line statement rule usedArguments arguments) isLast = fStatement <> " & " <> fRule <> fNewLine
  where
    fNewLine = if isLast then "" else " \\\\"
    fStatement = formatText statement
    fRule = "$" <> formatText rule <> "$ " <> T.intercalate ", " (take usedArguments arguments)
    formatText = T.pack . replaceLatexSymbols . T.unpack . replaceSpecialSymbols

-- Modified subproof formatting without bullets
formatStep (SubProof steps) _ = "\\begin{subproof}\n" <> formatSteps steps <> "\n\\end{subproof}"

-- Replace special symbols with LaTeX equivalents
replaceLatexSymbols :: String -> String
replaceLatexSymbols input =
  fixRuleNames $ fixArrows $ concatMap replaceSymbol input
  where
    replaceSymbol '&' = "\\land "
    replaceSymbol '|' = "\\lor "
    replaceSymbol '!' = "\\neg "
    replaceSymbol '¬' = "\\neg "
    replaceSymbol '→' = "\\rightarrow "
    replaceSymbol '⊥' = "\\bot "
    replaceSymbol '∧' = "\\land "
    replaceSymbol '⊢' = "\\vdash "
    replaceSymbol '∀' = "\\forall "
    replaceSymbol '∃' = "\\exists "
    replaceSymbol '₀' = "_0"
    replaceSymbol '₁' = "_1"
    replaceSymbol '₂' = "_2"
    replaceSymbol '₃' = "_3"
    replaceSymbol '₄' = "_4"
    replaceSymbol '₅' = "_5"
    replaceSymbol '₆' = "_6"
    replaceSymbol '₇' = "_7"
    replaceSymbol '₈' = "_8"
    replaceSymbol '₉' = "_9"
    replaceSymbol c = [c]

    -- Handle arrows separately
    fixArrows ('-':'>':rest) = "\\rightarrow " ++ fixArrows rest
    fixArrows (c:rest) = c : fixArrows rest
    fixArrows [] = []

    -- Fix rule names with proper LaTeX notation
    fixRuleNames = fixAndI . fixAndE . fixOrI . fixOrE . fixNotI . fixNotE . fixImplI . fixImplE . fixBotE . fixPBC . fixCopy . fixAllE . fixAllI . fixSomeE . fixSomeI . fixEqE . fixEqI . fixFresh . fixAssume

    -- Standard inference rules
    fixAndI = replace "AndI" "\\land \\mathrm{I}"                                            -- Conjunction Introduction
    fixAndE = replace "AndER" "\\land \\mathrm{ER}" . replace "AndEL" "\\land \\mathrm{EL}"  -- Conjunction Elimination
    fixOrI = replace "OrIR" "\\lor \\mathrm{IR}" . replace "OrIL" "\\lor \\mathrm{IL}"       -- Disjunction Introduction
    fixOrE = replace "OrE" "\\lor \\mathrm{E}"                                               -- Disjunction Elimination
    fixNotI = replace "NotI" "\\neg \\mathrm{I}"                                             -- Negation Introduction

    -- Fix various forms of negation elimination
    fixNotE = replaceAll [
        ("N otE", "\\neg \\mathrm{E}"),       -- Fix space issue
        ("NotE", "\\neg \\mathrm{E}"),        -- Standard format
        ("Not¬E", "\\neg \\mathrm{E}"),       -- Mixed format
        ("N ot¬E", "\\neg \\mathrm{E}"),      -- Mixed format with space
        ("¬E", "\\neg \\mathrm{E}")           -- Unicode symbol format
      ]

    -- Fix various forms of implication introduction
    fixImplI = replaceAll [
        ("IfI", "\\rightarrow \\mathrm{I}"),
        ("ImplI", "\\rightarrow \\mathrm{I}"),
        ("->I", "\\rightarrow \\mathrm{I}")
      ]

    -- Fix various forms of implication elimination
    fixImplE = replaceAll [
        ("IfE", "\\rightarrow \\mathrm{E}"),
        ("ImplE", "\\rightarrow \\mathrm{E}"),
        ("->E", "\\rightarrow \\mathrm{E}")
      ]

    fixBotE = replace "BotE" "\\bot \\mathrm{E}"   -- Bottom Elimination
    fixPBC = replace "PBC" "\\mathrm{PBC}"         -- Proof by Contradiction
    fixCopy = replace "copy" "\\mathrm{Copy}"      -- Copy rule

    fixAllE = replace "AllE" "\\forall \\mathrm{E}"
    fixAllI = replace "AllI" "\\forall \\mathrm{I}"

    fixSomeE = replace "SomeE" "\\exists \\mathrm{E}"
    fixSomeI = replace "SomeI" "\\exists \\mathrm{I}"

    fixEqE = replace "EqE" "= \\mathrm{E}"
    fixEqI = replace "EqI" "= \\mathrm{I}"
    
    fixFresh = replace "fresh" "\\mathrm{fresh}"
    fixAssume = replace "assume" "\\mathrm{assumption}"

    -- Helper to apply multiple replacements
    replaceAll :: [(String, String)] -> String -> String
    replaceAll [] str = str
    replaceAll ((old, new):rest) str = replaceAll rest (replace old new str)

-- Helper function to replace strings
replace :: String -> String -> String -> String
replace old new = go
  where
    go str@(c:cs)
      | take (length old) str == old = new ++ go (drop (length old) str)
      | otherwise = c : go cs
    go [] = []

-- Calculates maximum depth of subproofs
subProofDepth :: FESequent -> Integer
subProofDepth seq = maximum $ map (md 0) (_steps seq)
  where
    md :: Integer -> FEStep -> Integer
    md currentDepth (Line {}) = currentDepth
    md currentDepth (SubProof steps) = maximum (map (md (currentDepth + 1)) steps)