{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.Helper where

import Frontend.Types
import Shared.Messages
import Monomer
import qualified Monomer.Lens as L
import Data.Char (isSpace)
import Data.Text (Text, pack, unpack, intercalate, splitOn)
import Data.List (find, dropWhileEnd)
import Text.Printf
import System.Random
import System.FilePath.Posix (equalFilePath, takeDirectory)
import Backend.Types (Ref(RefRange, RefLine))

import Control.Exception (SomeException, catch)
import System.Process (callCommand)
import Control.Lens ((^.))

isFileEdited :: Maybe File -> Bool
isFileEdited (Just f@ProofFile {}) = _isEdited f
isFileEdited (Just f@TemporaryProofFile {}) = _isEdited f
isFileEdited (Just f@PreferenceFile {}) = _isEdited f
isFileEdited Nothing = False
isFileEdited _ = False

getProofFileByPath :: [File] -> FilePath -> Maybe File
getProofFileByPath allFiles filePath = find (\f -> _path f `equalFilePath` filePath) allFiles

evalPath :: FESequent -> FormulaPath -> FEStep
evalPath sequent formulaPath = ep formulaPath (SubProof $ _steps sequent)
  where
    ep (idx:rest) currentProof = case currentProof of
      SubProof p -> ep rest $ p !! idx
      Line {} -> error "Tried to index into `Line` (not an array)"
    ep [] p = p

pathToLineNumber :: FESequent -> FormulaPath -> Integer
pathToLineNumber sequent path = ep path (SubProof $ _steps sequent) 1
  where
    ep (idx:tail) currentProof startLine = case currentProof of
      SubProof p -> ep tail (p !! idx) (startLine + sum (map stepLength (take idx p)))
      Line {} -> error "Tried to index into `Line` (not an array)"
    ep [] _ startLine = startLine

    stepLength (SubProof p) = sum $ map stepLength p
    stepLength (Line {}) = 1

pathToLastLine :: FESequent -> FormulaPath
pathToLastLine sequent = ep (SubProof $ _steps sequent) []
  where
    ep currentProof path = case currentProof of
      Line {} -> path
      SubProof [] -> path
      SubProof p -> ep (last p) (path ++ [length p - 1])

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

showDecimals :: (PrintfArg t2) => Integer -> t2 -> Text
showDecimals decimals number = pack (printf "%0.*f" decimals number)

getTmpFileName :: IO String
getTmpFileName = do
  r01 <- randomIO :: IO Float
  let num = round (r01 * 1e6) :: Integer
  let t = "untitled_" <> show num
  return t

isErrorSubProof :: Integer -> Integer -> Maybe FEResult -> Bool
isErrorSubProof start end (Just (FEError _ (FELocal (RefRange a b) _))) = start == a && end == b
isErrorSubProof _ _ (Just (FEError _ (FELocal (RefLine _) _))) = False
isErrorSubProof _ _ (Just (FEError _ (FEGlobal {}))) = False
isErrorSubProof _ _ (Just (FEOk _)) = False
isErrorSubProof _ _ Nothing = False

isErrorLine :: Integer -> Maybe FEResult -> Bool
isErrorLine lineNumber (Just (FEError _ (FELocal (RefLine line) _))) = line == lineNumber
isErrorLine _ (Just (FEError _ (FELocal (RefRange _ _) _))) = False
isErrorLine _ (Just (FEError _ (FEGlobal {}))) = False
isErrorLine _ (Just (FEOk _)) = False
isErrorLine _ Nothing = False

extractErrorMsg :: Maybe FEResult -> String
extractErrorMsg Nothing = ""
extractErrorMsg (Just (FEOk _)) = ""
extractErrorMsg (Just (FEError _ (FELocal _ msg))) = msg
extractErrorMsg (Just (FEError _ (FEGlobal msg))) = msg

insertAt :: a -> Int -> [a] -> [a]
insertAt newElement _ [] = [newElement]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as

openInExplorer :: WidgetEnv s e -> FilePath -> IO ()
openInExplorer wenv path = catchIgnore (callCommand command) where
  os = wenv ^. L.os
  command
    | os == "Windows" = "start %windir%\\explorer.exe \"" ++ path ++ "\""
    | os == "Mac OS X" = "cd \"" ++ path ++ "\"; open -R ."
    | os == "Linux" = "xdg-open \"" ++ takeDirectory path ++ "\""
    | otherwise = "ls"

catchIgnore :: IO () -> IO ()
catchIgnore task = catchAny task (const $ return ())

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch