{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.Helper.General where

import Frontend.Types
import Shared.Messages
import Backend.Environment

import Monomer
import Monomer.Main.Platform (getPlatform)
import qualified Monomer.Lens as L
import Control.Lens
import Control.Exception (SomeException, catch)
import Data.Char (isSpace)
import Data.Text (Text, pack, unpack, intercalate, splitOn)
import Data.List (find, dropWhileEnd, isInfixOf)
import Text.Printf
import System.Random
import System.FilePath (equalFilePath, takeDirectory, (</>))
import System.Process (callCommand)
import System.Environment (getExecutablePath)

-- https://www.youtube.com/watch?v=aS8O-F0ICxw
-- | Safely gets head of list
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (h:_) = Just h

-- | Safely get element in list (safer than before)
maybeIndex :: [a] -> Int -> Maybe a
maybeIndex []     _ = Nothing
maybeIndex (a:_)  0 = Just a
maybeIndex (_:as) n
  | n > 0     = maybeIndex as (n-1)
  | otherwise = Nothing



{-|
Removes element at index from list.
The original list is returned if the index doesn't exist
-}
removeIdx :: Int -> [a] -> [a]
removeIdx idx lst
  | idx < 0 = lst
  | otherwise = part1 ++ drop 1 part2
  where (part1, part2) = splitAt idx lst

-- | Inserts an element into a list such that the new element has the given index
insertAt :: a -> Int -> [a] -> [a]
insertAt newElement _ [] = [newElement]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as

{-|
Takes part of list. Start is inclusive and end is exclusive.
Will not fail if start or end is outside the range of the list.
-}
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)

-- | Removes whitespace from start and end of `String`
trim :: [Char] -> [Char]
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Removes whitespace from start and end of `Text`
trimText :: Text -> Text
trimText = pack . trim . unpack

-- | Trims beginning of text only if it matches the given text
trimBeginning :: Text -> Text -> Text
trimBeginning match text
  | hasBeg = intercalate match (tail split)
  | otherwise = text
  where
    hasBeg = length split >= 2 && head split == ""
    split = splitOn match text

-- | Round value to a given number of decimal places
showDecimals :: (PrintfArg t2) => Integer -> t2 -> Text
showDecimals decimals number = pack (printf "%0.*f" decimals number)

-- | Split list into chunks of `n` elements
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = take n l : chunksOf n (drop n l)
  | otherwise = error "Negative or zero chunk size"




{-|
Returns a boolean indicating whether a file has been edited or not.
Returns false if the file doesn't support editing
-}
isFileEdited :: Maybe File -> Bool
isFileEdited (Just f@ProofFile {}) = _isEdited f
isFileEdited (Just f@TemporaryProofFile {}) = _isEdited f
isFileEdited (Just f@PreferenceFile {}) = _isEdited f
isFileEdited Nothing = False
isFileEdited _ = False

-- | Check if FilePath is new, unsaved file
isTmpFile :: FilePath -> Bool
isTmpFile filePath =
  ("/_tmp/" `isInfixOf` filePath) ||
  ("\\_tmp\\" `isInfixOf` filePath) ||
  ("/_tmp\\" `isInfixOf` filePath) ||
  ("\\_tmp/" `isInfixOf` filePath)

-- | Gets a `File` by it's (hopefully) unique `FilePath` in a given list of files
getProofFileByPath :: [File] -> FilePath -> Maybe File
getProofFileByPath allFiles filePath = find (\f -> _path f `equalFilePath` filePath) allFiles

-- | Generate path for sibling one step below path
nextSibling :: FormulaPath -> FormulaPath
nextSibling path = init path ++ [last path + 1]

-- | Generate path for sibling one step above path
prevSibling :: FormulaPath -> FormulaPath
prevSibling path = init path ++ [last path - 1]

-- | Get step from sequent by a given path
evalPath :: FormulaPath -> FESequent -> FEStep
evalPath formulaPath sequent = ep formulaPath (SubProof $ _steps sequent)
  where
    ep (idx:rest) currentProof = case currentProof of
      SubProof p -> ep rest $ p !! idx
      Line {} -> error "Tried to index into `Line` (not an array)"
    ep [] p = p

-- | Get step from sequent by a given path
evalPathSafe :: FormulaPath -> FESequent -> Maybe FEStep
evalPathSafe formulaPath sequent = ep formulaPath (SubProof $ _steps sequent)
  where
    ep (idx:rest) currentProof = case currentProof of
      SubProof p
        | idx >= length p -> Nothing
        | otherwise -> ep rest $ p !! idx
      Line {} -> Nothing
    ep [] p = Just p

{-|
Gives the line number for a given path. Premises are not accounted for.
Will return the first line in subproof if path points to subproof
-}
pathToLineNumber :: FESequent -> FormulaPath -> Integer
pathToLineNumber sequent path = ep path (SubProof $ _steps sequent) 1
  where
    ep (idx:tail) currentProof startLine = case currentProof of
      SubProof p -> ep tail (p !! idx) (startLine + sum (map proofStepLength (take idx p)))
      Line {} -> error "Tried to index into `Line` (not an array)"
    ep [] _ startLine = startLine

{-|
Gives the line number for a given path and accounts for premises.
Will return the first line in subproof if path points to subproof
-}
pathToLineNumberOffsetPremises :: FESequent -> FormulaPath -> Integer
pathToLineNumberOffsetPremises sequent path = pathToLineNumber sequent path + toInteger (length (_premises sequent))

-- | Get the length of a step
proofStepLength :: Num a => FEStep -> a
proofStepLength (SubProof p) = sum $ map proofStepLength p
proofStepLength (Line {}) = 1

-- | Get start and end line of path
lineNumberRange :: FormulaPath -> FESequent -> (Integer, Integer)
lineNumberRange path seq = (start, start + offset - 1)
  where
    start = pathToLineNumberOffsetPremises seq path
    offset = proofStepLength e
    e = evalPath path seq

-- | Get the path to the last line in a proof. Will always return the path to a line and not a subproof
pathToLastLine :: FESequent -> FormulaPath
pathToLastLine sequent = ep (SubProof $ _steps sequent) []
  where
    ep currentProof path = case currentProof of
      Line {} -> path
      SubProof [] -> path
      SubProof p -> ep (last p) (path ++ [length p - 1])

-- | Check if second path is contained in first path
pathIsParentOf :: FormulaPath -> FormulaPath -> Bool
pathIsParentOf parent child = parent == basePath where
  basePath = take (length parent) child

-- | Generate random temporary name for a new file
getTmpFileName :: IO String
getTmpFileName = do
  r01 <- randomIO :: IO Float
  let num = round (r01 * 1e6) :: Integer
  let t = "untitled_" <> show num
  return t

-- | Check if subproof (given startline and endline) contains the error returned from backend
isErrorSubProof :: Integer -> Integer -> Maybe FEResult -> Bool
isErrorSubProof start end (Just (FEError _ (FELocal (RefRange a b) _))) = start == a && end == b
isErrorSubProof _ _ (Just (FEError _ (FELocal (RefLine _) _))) = False
isErrorSubProof _ _ (Just (FEError _ (FEGlobal {}))) = False
isErrorSubProof _ _ (Just (FEOk _)) = False
isErrorSubProof _ _ Nothing = False

-- | Check if a line number contains the error returned from backend
isErrorLine :: Integer -> Maybe FEResult -> Bool
isErrorLine lineNumber (Just (FEError _ (FELocal (RefLine line) _))) = line == lineNumber
isErrorLine _ (Just (FEError _ (FELocal (RefRange _ _) _))) = False
isErrorLine _ (Just (FEError _ (FEGlobal {}))) = False
isErrorLine _ (Just (FEOk _)) = False
isErrorLine _ Nothing = False

-- | Get all warnings applicable to given line number from result from backend
getWarningsOnLine :: Integer -> Maybe FEResult -> [String]
getWarningsOnLine lineNumber (Just (FEError warns _)) = map getMsgFromWhere $ filter (whereIsOnLine lineNumber) warns
getWarningsOnLine lineNumber (Just (FEOk warns)) = map getMsgFromWhere $ filter (whereIsOnLine lineNumber) warns
getWarningsOnLine _ Nothing = []

-- | Get all warnings applicable to given subproof from result from backend
getWarningsInSubProof :: Integer -> Integer -> Maybe FEResult -> [String]
getWarningsInSubProof start end (Just (FEError warns _)) = map getMsgFromWhere $ filter (whereIsSubProof start end) warns
getWarningsInSubProof start end (Just (FEOk warns)) = map getMsgFromWhere $ filter (whereIsSubProof start end) warns
getWarningsInSubProof _ _ Nothing = []

-- | Check if error location matches given line number
whereIsOnLine :: Integer -> FEErrorWhere -> Bool
whereIsOnLine _ (FEGlobal {}) = False
whereIsOnLine _ (FELocal (RefRange {}) _) = False
whereIsOnLine lineNumber (FELocal (RefLine l) _) = l == lineNumber

-- | Check if error location matches given subproof range
whereIsSubProof :: Integer -> Integer -> FEErrorWhere -> Bool
whereIsSubProof _ _ (FEGlobal {}) = False
whereIsSubProof _ _ (FELocal (RefLine _) _) = False
whereIsSubProof start end (FELocal (RefRange s e) _) = start == s && end == e

-- | Extract message from error
getMsgFromWhere :: FEErrorWhere -> String
getMsgFromWhere (FEGlobal msg) = msg
getMsgFromWhere (FELocal _ msg) = msg

-- | Extract error message from result
extractErrorMsg :: Maybe FEResult -> String
extractErrorMsg Nothing = ""
extractErrorMsg (Just (FEOk _)) = ""
extractErrorMsg (Just (FEError _ (FELocal _ msg))) = msg
extractErrorMsg (Just (FEError _ (FEGlobal msg))) = msg

-- | Open given `FilePath` in the default file manager (Windows Exploror, Finder or File Manager etc)
openInExplorer :: WidgetEnv s e -> FilePath -> IO ()
openInExplorer wenv path = catchIgnore (callCommand command) where
  os = wenv ^. L.os
  command
    | os == "Windows" = "start %windir%\\explorer.exe /select,\"" ++ path ++ "\""
    | os == "Mac OS X" = "open -R \"" ++ path ++ "\""
    -- | os == "Mac OS X" = "cd \"" ++ takeDirectory path ++ "\"; open -R ."
    | os == "Linux" = "xdg-open \"" ++ takeDirectory path ++ "\""
    | otherwise = "ls"

-- | Perform IO action and ignore errors
catchIgnore :: IO () -> IO ()
catchIgnore task = catchAny task (const $ return ())

-- | Type to catch any exception
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

-- | Find directory where assets are placed
getAssetBasePath :: IO FilePath
getAssetBasePath = do
  os <- getPlatform
  let isMac = os == "Mac OS X"

  if isMac
    then do
      exePath <- getExecutablePath
      return $ takeDirectory exePath </> "../Resources/"
    else do
      return "./"

intToWarningSeverity :: Int -> Text
intToWarningSeverity i | i==1="All"
                       | i==2="Some"
                       | i==3="Severe"
                       | otherwise = intToWarningSeverity 1