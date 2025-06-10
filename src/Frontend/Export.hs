{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.Export (
  latexCompilerAvailable,
  convertToLatex,
  compileLatexToPDF
) where

import Data.Char (isAlphaNum, isDigit)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.List (isPrefixOf)
import Frontend.Types
import Shared.SpecialCharacters (replaceSpecialSymbols)
import Frontend.Helper.General (getProofFileByPath)
import Control.Lens ((^.))
import System.Directory (copyFile, doesFileExist)
import Control.Monad (unless)

import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readCreateProcessWithExitCode, proc)
import Control.Exception (try, SomeException)

-- | Check if pdflatex is installed
latexCompilerAvailable :: IO (Either String ())
latexCompilerAvailable = do
  result <- try (
      readCreateProcessWithExitCode (proc "pdflatex" ["-version"]) ""
    ) :: IO (Either SomeException (ExitCode, String, String))
  case result of
    Left ex -> return (Left (show ex))
    Right _ -> return (Right ())

-- | Compile a LaTeX file to PDF format
compileLatexToPDF :: FilePath -> Text -> AppModel -> IO (Either String FilePath)
compileLatexToPDF savePath title model = do
  withSystemTempDirectory "latex-temp" $ \tmpDir -> do
    let latexContent = convertToLatex title model
    let texName = "source.tex"
    let texPath = tmpDir </> texName
    let finalPdfPath = savePath <.> "pdf"

    -- Write the LaTeX content to the file
    writeResult <- try (writeFile texPath (unpack latexContent)) :: IO (Either SomeException ())
    case writeResult of
      Left ex -> do
        putStrLn ("write failed: " ++ show ex)
        return $ Left (show ex)

      Right () -> do
        putStrLn $ "Starting LaTeX compilation for: " ++ texPath
        result <- try (
            readCreateProcessWithExitCode (proc "pdflatex" ["-interaction=nonstopmode", "-file-line-error", "-output-directory", tmpDir, texName]) ""
          ) :: IO (Either SomeException (ExitCode, String, String))
        case result of
          Left ex -> do
            putStrLn ("process failed: " ++ show ex)
            return $ Left (show ex)

          Right (_exitCode, _stdout, stderr) -> do
            unless (null stderr) $ putStrLn $ "Errors: " ++ stderr

            let tmpPdfPath = tmpDir </> "source.pdf"
            putStrLn $ "Looking for PDF at: " ++ tmpPdfPath
            
            pdfExists <- doesFileExist tmpPdfPath
            
            if pdfExists
              then do
                putStrLn $ "Copying PDF from: " ++ tmpPdfPath ++ " to: " ++ finalPdfPath
                copyFile tmpPdfPath finalPdfPath
                finalExists <- doesFileExist finalPdfPath
                if finalExists
                  then return $ Right finalPdfPath
                  else return $ Left "Failed to copy PDF to final destination"
              else 
                return $ Left $ "PDF was not generated: " ++ stderr

-- | Convert the proof model to LaTeX format
convertToLatex :: Text -> AppModel -> Text
convertToLatex title model = T.unlines
  [ "\\documentclass{article}"
  , "\\usepackage{amsmath}"
  , "\\usepackage{amssymb}"
  , "\\usepackage{logicproof}"
  , ""
  , "\\begin{document}"
  , ""
  , "\\begin{center}"
  , section
  , "\\end{center}"
  , ""
  , formatProof model
  , ""
  , "\\end{document}"
  ]
  where
    section
      | title == "" = ""
      | otherwise = "\\section*{" <> title <> "}"

-- | Format the proof as LaTeX
formatProof :: AppModel -> Text
formatProof model = maybe "% No proof available" formatSequent currentSeq
  where
    currentSeq = do
      filePath <- model ^. persistentState . currentFile
      file <- getProofFileByPath (model ^. persistentState . tmpLoadedFiles) filePath
      case file of
        ProofFile {_parsedDocument = Just s} -> Just $ _sequent s
        TemporaryProofFile {_parsedDocument = Just s} -> Just $ _sequent s
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

    premiseLines = if null (_premises seq)
                   then ""
                   else T.intercalate "\n" (map premiseLine (_premises seq))

    premiseLine premise = formatFormula premise <> " & premise \\\\"

    formatFormula :: Text -> Text
    formatFormula = T.pack . fixSpaces . replaceLatexSymbols . T.unpack . replaceSpecialSymbols
       where
          fixSpaces = unwords . words

formatSteps :: [FEStep] -> Text
formatSteps steps = T.intercalate "\n" (zipWith (\s i -> formatStep s (i == length steps)) steps [1..])

formatStep :: FEStep -> Bool -> Text
formatStep (Line statement rule usedArguments arguments) isLast = fStatement <> " & " <> fRule <> fNewLine
  where
    fNewLine = if isLast then "" else " \\\\"
    fStatement = formatText statement
    fRule = "$" <> formatText rule <> "$ " <> T.intercalate ", " (map formatArgument (take usedArguments arguments))
    formatText = T.pack . replaceLatexSymbols . T.unpack . replaceSpecialSymbols
    formatArgument arg = T.pack . fixSubscriptsOnly . T.unpack $ arg
    
    fixSubscriptsOnly [] = []
    fixSubscriptsOnly ('x':'₀':rest) = "x_{0}" ++ fixSubscriptsOnly rest
    fixSubscriptsOnly ('x':'₁':rest) = "x_{1}" ++ fixSubscriptsOnly rest
    fixSubscriptsOnly ('x':'₂':rest) = "x_{2}" ++ fixSubscriptsOnly rest
    fixSubscriptsOnly ('x':'₃':rest) = "x_{3}" ++ fixSubscriptsOnly rest
    fixSubscriptsOnly ('x':'₄':rest) = "x_{4}" ++ fixSubscriptsOnly rest
    fixSubscriptsOnly ('x':'₅':rest) = "x_{5}" ++ fixSubscriptsOnly rest
    fixSubscriptsOnly ('y':'₀':rest) = "y_{0}" ++ fixSubscriptsOnly rest
    fixSubscriptsOnly ('y':'₁':rest) = "y_{1}" ++ fixSubscriptsOnly rest
    fixSubscriptsOnly (c:rest) = c : fixSubscriptsOnly rest

formatStep (SubProof steps) _ = "\\begin{subproof}\n" <> formatSteps steps <> "\n\\end{subproof}"

-- | Replace special symbols with LaTeX equivalents
replaceLatexSymbols :: String -> String
replaceLatexSymbols input =
  fixSubscripts $ fixRuleNames $ fixArrows $ concatMap replaceSymbol input
  where
    replaceSymbol '&' = "\\land "
    replaceSymbol '|' = "\\lor "
    replaceSymbol '!' = "\\neg "
    replaceSymbol '∨' = "\\lor "
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
    replaceSymbol ' ' = " "
    replaceSymbol c = [c]

    fixArrows ('-':'>':rest) = "\\rightarrow " ++ fixArrows rest
    fixArrows (c:rest) = c : fixArrows rest
    fixArrows [] = []

    fixSubscripts [] = []
    fixSubscripts (c:'_':d:rest) 
      | isAlphaNum c && isDigit d = 
          c : "_{" ++ d : digits ++ "}" ++ fixSubscripts rest'
      where
        (digits, rest') = span isDigit rest
    fixSubscripts ('_':d:rest)
      | isDigit d = 
          "_{" ++ d : digits ++ "}" ++ fixSubscripts rest'
      where
        (digits, rest') = span isDigit rest
    fixSubscripts (c:rest) = c : fixSubscripts rest

    fixRuleNames = fixAndI . fixAndE . fixOrI . fixOrE . fixNotI . fixNotE . fixImplI . fixImplE . fixBotE . fixPBC . fixCopy . fixAllE . fixAllI . fixSomeE . fixSomeI . fixEqE . fixEqI . fixFresh . fixAssume

    fixAndI = replace "AndI" "\\land \\mathrm{I}"
    fixAndE = replace "AndER" "\\land \\mathrm{ER}" . replace "AndEL" "\\land \\mathrm{EL}"
    fixOrI = replace "OrIR" "\\lor \\mathrm{IR}" . replace "OrIL" "\\lor \\mathrm{IL}"
    fixOrE = replace "OrE" "\\lor \\mathrm{E}"
    fixNotI = replace "NotI" "\\neg \\mathrm{I}"
    fixNotE = replaceAll [
        ("N otE", "\\neg \\mathrm{E}"),
        ("NotE", "\\neg \\mathrm{E}"),
        ("Not¬E", "\\neg \\mathrm{E}"),
        ("N ot¬E", "\\neg \\mathrm{E}"),
        ("¬E", "\\neg \\mathrm{E}")
      ]
    fixImplI = replaceAll [
        ("IfI", "\\rightarrow \\mathrm{I}"),
        ("ImplI", "\\rightarrow \\mathrm{I}"),
        ("->I", "\\rightarrow \\mathrm{I}")
      ]
    fixImplE = replaceAll [
        ("IfE", "\\rightarrow \\mathrm{E}"),
        ("ImplE", "\\rightarrow \\mathrm{E}"),
        ("->E", "\\rightarrow \\mathrm{E}")
      ]
    fixBotE = replace "BotE" "\\bot \\mathrm{E}"
    fixPBC = replace "PBC" "\\mathrm{PBC}"
    fixCopy = replace "copy" "\\mathrm{Copy}"
    fixAllE = replace "AllE" "\\forall \\mathrm{E}"
    fixAllI = replace "AllI" "\\forall \\mathrm{I}"
    fixSomeE = replace "SomeE" "\\exists \\mathrm{E}"
    fixSomeI = replace "SomeI" "\\exists \\mathrm{I}"
    fixEqE = replace "EqE" "= \\mathrm{E}"
    fixEqI = replace "EqI" "= \\mathrm{I}"
    fixFresh = replace "fresh" "\\mathrm{fresh}"
    fixAssume = replace "assume" "\\mathrm{assumption}"

replace :: String -> String -> String -> String
replace old new = go
  where
    go [] = []
    go str@(c:cs)
      | old `isPrefixOf` str = new ++ go (drop (length old) str)
      | otherwise = c : go cs

replaceAll :: [(String, String)] -> String -> String
replaceAll replacements text = foldl (\t (old, new) -> replace old new t) text replacements

subProofDepth :: FESequent -> Integer
subProofDepth seq = maximum $ map (md 0) (_steps seq)
  where
    md :: Integer -> FEStep -> Integer
    md currentDepth (Line {}) = currentDepth
    md currentDepth (SubProof steps) = maximum (map (md (currentDepth + 1)) steps)
