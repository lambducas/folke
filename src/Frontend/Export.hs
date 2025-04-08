module Frontend.Export
  ( convertToLatex
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Frontend.Types
import Frontend.SpecialCharacters (replaceSpecialSymbols)
import Backend.Types
import Frontend.Helper (getProofFileByPath)
import Control.Lens ((^.))
import System.FilePath (takeBaseName)

-- | Convert the proof model to LaTeX format
convertToLatex :: AppModel -> Text
convertToLatex model = T.unlines
  [ "\\documentclass{article}"
  , "\\usepackage{amsmath}"
  , "\\usepackage{amssymb}" -- For \ulcorner symbol
  , "\\usepackage{mathtools}"
  , "\\usepackage{tikz}"
  , ""
  , "% Custom styles for proof formatting"
  , "\\setlength{\\parindent}{0pt}"
  , "\\pagestyle{empty}" -- Remove page numbers
  , "\\newcommand{\\proofline}[2]{$#1$ \\hfill $[#2]$\\\\[0.5em]}"
  , "\\newcommand{\\proofstatement}[1]{$#1$\\\\[0.5em]}"
  , "% Corner brackets for subproofs"
  , "\\newenvironment{subproof}{%"
  , "  \\begin{minipage}{\\textwidth}%"
  , "  $\\ulcorner$ \\begin{addmargin}[2em]{0em}%"
  , "}{\\end{addmargin}\\end{minipage}\\\\[0.5em]}"
  , "\\usepackage{scrextend}" -- For addmargin environment
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
    sectionTitle = case model ^. currentFile of
      Nothing -> "Formal Proof"
      Just filePath -> "Some submission" -- T.pack $ takeBaseName filePath

-- Format the proof as LaTeX
formatProof :: AppModel -> Text
formatProof model = 
  case currentSeq of
    Nothing -> "% No proof available"
    Just seq -> formatSequent seq
  where
    currentSeq = do
      filePath <- model ^. currentFile
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
  , premiseLines
  , formatSteps (_steps seq)
  ]
  where
    formattedSequent = 
      if null (_premises seq)
        then "\\vdash " <> formatFormula (_conclusion seq)
        else T.intercalate ", " (map formatFormula (_premises seq)) <> 
             " \\vdash " <> formatFormula (_conclusion seq)
    
    -- Add the premises as initial lines in the proof (without numbering)
    premiseLines = if null (_premises seq)
                   then ""
                   else T.unlines (map premiseLine (_premises seq))
    
    premiseLine premise = "\\proofline{" <> formatFormula premise <> "}{premise}"
    
    formatFormula = T.pack . replaceLatexSymbols . T.unpack . replaceSpecialSymbols

-- Format steps with proper enumeration
formatSteps :: [FEStep] -> Text
formatSteps steps = T.unlines (map formatStep steps)

-- Format a single step (with justification but without numbers)
formatStep :: FEStep -> Text
formatStep (Line statement "") =
  "\\proofstatement{" <> formatText statement <> "}"
  where
    formatText = T.pack . replaceLatexSymbols . T.unpack . replaceSpecialSymbols

formatStep (Line statement rule) =
  "\\proofline{" <> formatText statement <> "}{" <> formatText rule <> "}"
  where
    formatText = T.pack . replaceLatexSymbols . T.unpack . replaceSpecialSymbols

-- Modified subproof formatting without bullets
formatStep (SubProof steps) =
  T.unlines
  [ "\\begin{subproof}"
  , formatSteps steps
  , "\\end{subproof}"
  ]

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
    replaceSymbol c = [c]

    -- Handle arrows separately
    fixArrows ('-':'>':rest) = "\\rightarrow " ++ fixArrows rest
    fixArrows (c:rest) = c : fixArrows rest
    fixArrows [] = []

    -- Fix rule names with proper LaTeX notation
    fixRuleNames = fixAndI . fixAndE . fixOrI . fixOrE . fixNotI . fixNotE . fixImplI . fixImplE . fixBotE . fixPBC . fixCopy
    
    -- Standard inference rules
    fixAndI = replace "AndI" "\\land I"  -- Conjunction Introduction
    fixAndE = replace "AndE" "\\land E"  -- Conjunction Elimination
    fixOrI = replace "OrI" "\\lor I"     -- Disjunction Introduction
    fixOrE = replace "OrE" "\\lor E"     -- Disjunction Elimination
    fixNotI = replace "NotI" "\\neg I"   -- Negation Introduction
    
    -- Fix various forms of negation elimination
    fixNotE = replaceAll [
        ("N otE", "\\neg E"),       -- Fix space issue
        ("NotE", "\\neg E"),        -- Standard format
        ("Not¬E", "\\neg E"),       -- Mixed format
        ("N ot¬E", "\\neg E"),      -- Mixed format with space
        ("¬E", "\\neg E")           -- Unicode symbol format
      ]
    
    -- Fix various forms of implication introduction
    fixImplI = replaceAll [
        ("IfI", "\\rightarrow I"),
        ("ImplI", "\\rightarrow I"),
        ("->I", "\\rightarrow I")
      ]
    
    -- Fix various forms of implication elimination
    fixImplE = replaceAll [
        ("IfE", "\\rightarrow E"),
        ("ImplE", "\\rightarrow E"),
        ("->E", "\\rightarrow E")
      ]
    
    fixBotE = replace "BotE" "\\bot E"   -- Bottom Elimination
    fixPBC = replace "PBC" "PBC"         -- Proof by Contradiction (keep as is)
    fixCopy = replace "copy" "Copy"      -- Copy rule

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