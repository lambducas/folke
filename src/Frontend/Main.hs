{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Frontend.Main where

import System.Directory
import Control.Concurrent (threadDelay, Chan, newChan)
import Control.Lens
import Monomer
import TextShow
import Data.Text (Text, replace, unpack, pack, intercalate, splitOn)
import Data.List (find, dropWhileEnd)
import qualified Data.Maybe

import Shared.Messages
import Backend.TypeChecker (isProofCorrect)
import Parser.Logic.Abs (Sequent(..), Form(..))
import Frontend.Communication (startCommunication, evaluateProofSegment)
import Data.Char (isSpace)

type SymbolDict = [(Text, Text)]
type FormulaPath = [Int]

data ProofFormula
  = MainProof [ProofFormula]
  | Formula {
    _statement :: Text,
    _rule :: Text
  }
  | SubProof [ProofFormula]
  | Removed
  deriving (Eq, Show)

data File = File {
  _path :: FilePath,
  _name :: Text,
  _subname :: Text,
  _content :: Text,
  _parsedContent :: ProofFormula
} deriving (Eq, Show)

data AppModel = AppModel {
  _clickCount :: Int,

  _newFilePopupOpen :: Bool,
  _newFileName :: Text,
  _loadedFiles :: [File],
  _tmpLoadedFiles :: [File],
  _openFiles :: [FilePath],
  _currentFile :: Maybe FilePath,

  _conclusion :: Text,
  _proofFormulas :: ProofFormula,

  _frontendChan :: Chan FrontendMessage,
  _backendChan :: Chan BackendMessage,
  _proofStatus :: Maybe Bool
} deriving (Eq, Show)

instance Show (Chan a) where
  show :: Chan a -> String
  show _ = ""

data AppEvent
  = AppInit
  | AppIncrease
  | NextFocus Int
  | AddLine
  | RemoveLine FormulaPath
  | EditLine FormulaPath Int Text
  | OutdentLine Int
  | IndentLine Int
  | SetLoadedFiles [File]
  | OpenFile File
  | CloseFile FilePath
  | SaveProof File
  | SetCurrentFile FilePath
  | OpenCreateProofPopup
  | CreateEmptyProof Text

  | CheckProof
  | BackendResponse BackendMessage
  deriving (Eq, Show)

makeLenses 'File
makeLenses 'AppModel

symbolLookup :: SymbolDict
symbolLookup = [
  ("!", "¬"),
  ("->", "→"),
  ("&", "∧"),
  ("|", "∨"),
  ("bot", "⊥"),
  ("#", "⊥"),
  ("forall", "∀"),
  ("exists", "∃"),
  ("=/>", "⊬"),
  ("=>", "⊢")
  ]

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI _wenv model = widgetTree where
  widgetTree = hstack [
      fileWindow,
      editWindow
    ]

  fileWindow = vstack [
      box (label "Manage proofs") `styleBasic` [padding 10],
      vstack $ map fileItem (model ^. loadedFiles),
      spacer,

      box $ button "+ New proof" OpenCreateProofPopup `styleBasic` [padding 10],
      popup newFilePopupOpen (vstack [
        label "This will appear on top of the widget tree",
        spacer,
        textField newFileName,
        spacer,
        let cep = (CreateEmptyProof $ model ^. newFileName) in
          keystroke [("Enter", cep)] $ button "Create proof" cep
      ] `styleBasic` [bgColor gray, padding 10])
    ] `styleBasic` [ width 250, borderR 1 gray ]

  fileItem file = box_ [expandContent, onClick (OpenFile file)] $ vstack [
      label $ _name file,
      label $ _subname file,
      label $ "Path: " <> pack (_path file)
    ] `styleBasic` [borderB 1 gray, padding 8, bgColor darkGray, cursorHand]

  fileNavBar filePaths = hscroll (hstack (map boxedLabel filePaths))
    `styleBasic` [bgColor black, maxHeight 50, minHeight 50, height 50]
    where
      boxedLabel filePath = hstack [
          button (showt filePath) (SetCurrentFile filePath) `styleBasic` [textColor white, bgColor transparent, paddingV 8, paddingH 16, radius 0, border 0 transparent],
          button "x" (CloseFile filePath) `styleBasic` [textColor white, bgColor transparent, radius 0, border 0 transparent]
        ] `styleBasic` [bgColor darkGray, border 1 gray, styleIf isCurrent (bgColor darkSlateGray), styleIf isCurrent (borderB 0 transparent)]
          where isCurrent = (model ^. currentFile) == Just filePath

  editWindow = vstack [
      fileNavBar (model ^. openFiles),
      widgetIf (Data.Maybe.isJust (model ^. currentFile)) (proofWindow $ model ^. currentFile)
    ]

  proofWindow Nothing = label "No proof selected"
  proofWindow (Just fileName) = case file of
    Nothing -> label "Filepath not loaded"
    Just file -> vstack [
        h1 $ _name file,
        label $ _subname file,
        spacer,
        -- label "→ ¬ ∧ ∨ ⊕ ⊥ ∀ ∃ ⊢ ⊬ ⟛",
        -- label $ replaceSpecialSymbols "P -> Q & L",
        -- vscroll $ label_ (exportProof model) [multiline],

        hstack [
          label "Conclusion",
          spacer,
          textField conclusion
        ] `styleBasic` [paddingV 8],
        spacer,

        vscroll $ vstack [
          fst $ proofTreeUI (model ^. proofFormulas),
          spacer,
          button "+ New line" AddLine `styleBasic` [ maxWidth 150 ]
        ],
        spacer,

        hstack [
          proofStatusLabel,
          spacer,
          button "Save proof" (SaveProof file)
        ]
      ] `styleBasic` [padding 10]
    where file = getProofFileByPath model fileName

  proofStatusLabel = hstack [
      widgetIf (model ^. proofStatus == Just True) (label "Proof is correct :)" `styleBasic` [textColor lime]),
      widgetIf (model ^. proofStatus == Just False) (label "Proof is not correct!" `styleBasic` [textColor pink]),
      widgetIf (model ^. proofStatus == Nothing) (label "Checking proof..." `styleBasic` [textColor orange])
    ]

  proofTreeUI :: ProofFormula -> (WidgetNode AppModel AppEvent, Integer)
  proofTreeUI formulas = pf formulas 1 []
    where
      pf :: ProofFormula -> Integer -> FormulaPath -> (WidgetNode AppModel AppEvent, Integer)
      pf (MainProof formulas) index path = (ui, lastIndex)
        where
          ui = vstack_ [childSpacing] (map fst s)
          lastIndex = if null s then index else snd $ last s
          s = getSubProof formulas path 0 index

      pf (SubProof p) index path = (ui, lastIndex)
        where
          ui = vstack_ [childSpacing] (map fst s) `styleBasic` [border 1 white, paddingV 8, paddingL 24]
          lastIndex = if null s then index else snd $ last s
          s = getSubProof p path 0 index

      pf (Formula statement rule) index path = (ui, lastIndex)
        where
          ui = hstack [
              label $ showt index <> ".",
              spacer,

              textFieldV (replaceSpecialSymbols statement) (EditLine path 0) `styleBasic` [textFont "Symbol_Regular"],
              spacer,

              textFieldV (replaceSpecialSymbols rule) (EditLine path 1) `styleBasic` [textFont "Symbol_Regular", width 175],
              spacer,

              trashButton (RemoveLine path)
              -- label $ showt index <> ".  " <> statement <> " : " <> rule
            ]
          lastIndex = index + 1

      pf Removed index _ = (label "`Removed` should not appear", index)

      getSubProof p path arrayIndex visualIndex
        | arrayIndex < length p = u : getSubProof p path (arrayIndex + 1) (snd u)
        | otherwise = []
          where u = pf (p !! arrayIndex) visualIndex (path ++ [arrayIndex])

directoryFilesProducer :: (AppEvent -> IO ()) -> IO ()
directoryFilesProducer sendMsg = do
  allFileNames <- listDirectory "./myProofs"
  allFiles <- traverse readProofFile allFileNames
  sendMsg (SetLoadedFiles allFiles)

  threadDelay $ 2 * seconds
  directoryFilesProducer sendMsg
    where
      seconds = 1000 * 1000
      readProofFile path = do
        let pName = pack path
            pSubname = "subname"
        pContent <- readFile ("./myProofs/" <> path)
        let pContentText = pack pContent
            parsedContent = parseProofFromFile pContentText
        return $ File path pName pSubname pContentText parsedContent

evaluateCurrentProof :: AppModel -> IO (Either String Sequent)
evaluateCurrentProof model = do
    let sequent = exportProof model
    evaluateProofSegment (model ^. frontendChan) (model ^. backendChan) sequent

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent _wenv _node model evt = case evt of
  AppInit -> [
      Producer directoryFilesProducer,
      Task $ do
        frontendChan <- newChan
        backendChan <- newChan
        startCommunication frontendChan backendChan
        return $ BackendResponse (OtherBackendMessage "Initialized")
    ]
  AppIncrease -> [Model (model & clickCount +~ 1)]

  -- Bruh
  NextFocus 1 -> [
      MoveFocusFromKey Nothing FocusFwd
    ]
  NextFocus 4 -> [
      MoveFocusFromKey Nothing FocusFwd,
      MoveFocusFromKey Nothing FocusFwd,
      MoveFocusFromKey Nothing FocusFwd,
      MoveFocusFromKey Nothing FocusFwd
    ]

  AddLine -> [
      Model $ model & proofFormulas .~ addLine (model ^. proofFormulas)
    ]
    where
      addLine (MainProof p) = MainProof $ p ++ [newLine]
      addLine _ = error "Root must be a `MainProof`"
      newLine = Formula "" ""

  RemoveLine path -> [
      Model $ model & proofFormulas .~ removeLine path (model ^. proofFormulas)
    ]
    where
      removeLine removePath = rl removePath []
      rl removePath currentPath (MainProof p)
        | removePath == currentPath = Removed
        | otherwise = MainProof $ filterRemoved $ zipWith (\p idx -> rl removePath (currentPath ++ [idx]) p) p [0..]
      rl removePath currentPath (SubProof p)
        | removePath == currentPath = Removed
        | otherwise = SubProof $ filterRemoved $ zipWith (\p idx -> rl removePath (currentPath ++ [idx]) p) p [0..]
      rl removePath currentPath f@(Formula _ _)
        | removePath == currentPath = Removed
        | otherwise = f
      rl _ _ p = p
      filterRemoved = filter (/=Removed)

  EditLine path arg newText -> [
      Model $ model & proofFormulas .~ editLine path arg newText (model ^. proofFormulas)
    ]
    where
      editLine editPath arg newText = el editPath arg newText []
      el editPath arg newText currentPath (MainProof p) = MainProof $ zipWith (\p idx -> el editPath arg newText (currentPath ++ [idx]) p) p [0..]
      el editPath arg newText currentPath (SubProof p) = SubProof $ zipWith (\p idx -> el editPath arg newText (currentPath ++ [idx]) p) p [0..]
      el editPath arg newText currentPath f@(Formula statement rule)
        | editPath == currentPath = case arg of
          0 -> Formula newText rule
          1 -> Formula statement newText
          _ -> error "Invalid field, should be 0 or 1"
        | otherwise = f
      el _ _ _ _ p = p

  OpenCreateProofPopup -> [
      Model $ model & newFilePopupOpen .~ True
    ]

  CreateEmptyProof fileName -> [
      Producer (\_ -> writeFile ("./myProofs/" <> unpack fileName <> ".logic") ""),
      Model $ model
        & newFilePopupOpen .~ False
        & newFileName .~ ""
    ]

  SetLoadedFiles fs -> [
      Model $ model
        & loadedFiles .~ fs
        & tmpLoadedFiles .~ fs
      -- , Producer (\_ -> print fs)
    ]

  OpenFile f -> Model newModel : handleEvent _wenv _node newModel (SetCurrentFile fileName)
    where
      fileName = _path f
      newModel = model
        & openFiles .~ (model ^. openFiles ++ [fileName | fileName `notElem` model ^. openFiles])

  CloseFile fileName -> [
      Model $ model
        & openFiles .~ filter (fileName/=) (model ^. openFiles)
        & currentFile .~ (if c == Just fileName then maybeHead (model ^. openFiles) else c)
    ]
    where c = model ^. currentFile

  SaveProof f -> [
      Producer (\_ -> writeFile ("./myProofs/" <> fileName) content)
    ]
    where
      content = unpack $ parseProofToFile $ model ^. proofFormulas
      fileName = _path f

  SetCurrentFile filePath -> case file of
    Nothing -> []
    Just file -> [
        Model $ model
          & (currentFile ?~ filePath)
          & proofFormulas .~ _parsedContent file
      ]
    where file = getProofFileByPath model filePath

  BackendResponse (SequentChecked result) -> case result of
    Left err -> [Message "Error" (pack err)]  -- Add type annotation
    Right sequent -> [Model $ model & proofStatus ?~ isProofCorrect sequent]

  BackendResponse (StepChecked result) -> case result of
    Left err -> [Message "Error" (pack err)]  -- Add type annotation
    Right _step -> [Message "Step Status" ("Step is correct" :: Text)]  -- Add type annotation

  f -> [
      Producer (\_ -> print f)
    ]

main :: IO ()
main = do
  frontendChan <- newChan
  backendChan <- newChan
  startCommunication frontendChan backendChan
  startApp (model frontendChan backendChan) handleEvent buildUI config
  where
    config = [
      appWindowTitle "● proof.logic - Proof Editor",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,

      appFontDef "Regular" "./assets/fonts/MPLUS1p/MPLUS1p-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/MPLUS1p/MPLUS1p-Medium.ttf",
      appFontDef "Bold" "./assets/fonts/MPLUS1p/MPLUS1p-Bold.ttf",

      appFontDef "Symbol_Regular" "./assets/fonts/JuliaMono/JuliaMono-Regular.ttf",
      appFontDef "Symbol_ Medium" "./assets/fonts/JuliaMono/JuliaMono-Medium.ttf",
      appFontDef "Symbol_Bold" "./assets/fonts/JuliaMono/JuliaMono-Bold.ttf",

      appFontDef "Remix" "./assets/fonts/remixicon.ttf",

      appInitEvent AppInit,
      appModelFingerprint show
      ]
    model frontendChan backendChan = AppModel {
      _clickCount = 0,
      _newFileName = "",
      _newFilePopupOpen = False,
      _loadedFiles = [],
      _tmpLoadedFiles = [],
      _openFiles = [],
      _currentFile = Nothing,
      _conclusion = "((P -> Q) & (!R -> !Q)) -> (P -> R)",
      _proofFormulas = MainProof [],

      _frontendChan = frontendChan,
      _backendChan = backendChan,
      _proofStatus = Nothing  -- Initialize proof status
    }

h1 :: Text -> WidgetNode s e
h1 t = label t `styleBasic` [ textSize 24, textFont "Bold" ]

iconButton :: Text -> AppEvent -> WidgetNode AppModel AppEvent
iconButton iconIdent action = button iconIdent action
  `styleBasic` [textFont "Remix", textMiddle, textColor orangeRed, bgColor transparent, border 0 transparent]

trashButton :: AppEvent -> WidgetNode AppModel AppEvent
trashButton = iconButton remixDeleteBinFill

getProofFileByPath :: AppModel -> FilePath -> Maybe File
getProofFileByPath model filePath = find (\f -> _path f == filePath) (model ^. loadedFiles)

-- Empty for now
exportProof :: AppModel -> Sequent
exportProof = const $ Seq [] FormBot []

-- exportProof model = Seq [] (FormPred (Pred (PredId (unpack (model ^. conclusion))) (Params []))) (map toStep (model ^. proofLines))

-- toStep :: ProofLine -> Step
-- toStep line = StepPrem (FormPred (Pred (PredId (unpack (line ^. statement))) (Params [])))

-- Placeholder
parseProofFromFile :: Text -> ProofFormula
parseProofFromFile p = case proof of
  [SubProof p] -> MainProof p
  [Removed] -> MainProof []
  _ -> error "Invalid proof from `parseText`"
  where
    proof = [parseText (unpack p) 0 []]

    parseText :: String -> Int -> [ProofFormula] -> ProofFormula
    parseText text ptr formulas = case nextSpecialChar of
      Just (char, idx) -> case char of
        ';' -> parseText text idx (formulas ++ [parseFormula $ pack $ slice (ptr + 1) idx text])
        '{' -> parseText text (findClosingBracket text 0 idx 0 + 1) (formulas ++ [parseText (slice (idx + 1) (findClosingBracket text 0 idx 0 + 1) text) 0 []])
        '}' -> SubProof formulas
        _ -> error "Invalid special char"
      Nothing -> Removed
      where
        nextSpecialChar = gotoNextChar text ptr ['{', '}', ';']

    parseFormula :: Text -> ProofFormula
    parseFormula text = Formula statement rule
      where
        statement = (pack . trim . unpack) $ head parts
        rule = (pack . trim . unpack) $ parts !! 1
        parts = splitOn ":" text

    gotoNextChar text ptr chars
      | ptr >= len - 1 = Nothing
      | currentChar `elem` chars = Just (currentChar, ptr + 1)
      | otherwise = gotoNextChar text (ptr + 1) chars
      where
        len = length text
        currentChar = text !! (ptr + 1)

    findClosingBracket :: [Char] -> Int -> Int -> Int -> Int
    findClosingBracket text nestedLevel idx cnl
      | idx >= length text - 1 = error "No closing bracket found"
      | otherwise = case char of
      '{' -> findClosingBracket text nestedLevel (idx + 1) (cnl + 1)
      '}' -> if nestedLevel == cnl then idx + 1 else findClosingBracket text nestedLevel (idx + 1) (cnl - 1)
      _ -> findClosingBracket text nestedLevel (idx + 1) cnl
      where char = text !! (idx + 1)

-- Placeholder
parseProofToFile :: ProofFormula -> Text
parseProofToFile p = exportProofHelper p 0
  where
    exportProofHelper :: ProofFormula -> Int -> Text
    exportProofHelper (MainProof p) indent = exportProofHelper (SubProof p) indent
    exportProofHelper (SubProof p) indent = tabs indent <> "{\n" <> intercalate "\n" (map (`exportProofHelper` (indent + 1)) p) <> "\n" <> tabs indent <> "}"
    exportProofHelper (Formula statement rule) indent = tabs indent <> statement <> " : " <> rule <> ";"
    exportProofHelper Removed _ = ""

    tabs :: Int -> Text
    tabs n = pack $ replicate n '\t'

replaceFromLookup :: Text -> SymbolDict -> Text
replaceFromLookup s [] = s
replaceFromLookup s ((key, value):ls) = replace key value $ replaceFromLookup s ls

replaceSpecialSymbols :: Text -> Text
replaceSpecialSymbols s = replaceFromLookup s symbolLookup

removeIdx :: Int -> [a] -> [a]
removeIdx idx lst = part1 ++ drop 1 part2 where
  (part1, part2) = splitAt idx lst

-- https://www.youtube.com/watch?v=aS8O-F0ICxw
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (h:_) = Just h

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)

trim :: [Char] -> [Char]
trim = dropWhileEnd isSpace . dropWhile isSpace