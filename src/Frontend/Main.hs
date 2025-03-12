{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Frontend.Main where

import Frontend.Types

import Monomer
import qualified Monomer.Lens as L
import Monomer.Core.Themes.BaseTheme
import Control.Lens
import Control.Concurrent (threadDelay, Chan, newChan)
import Control.Exception (try, SomeException (SomeException))
import TextShow ( TextShow(showt) )
import System.Directory ( doesFileExist, listDirectory )
import Data.Text (Text, replace, unpack, pack, intercalate, splitOn)
import Data.List (find, dropWhileEnd, findIndex)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, isNothing)
import Data.Default ( Default(def) )

import Shared.Messages
import Backend.TypeChecker (isProofCorrect)
import Parser.Logic.Abs (Sequent(..), Form(..))
import Frontend.Communication (startCommunication, evaluateProofSegment)

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
buildUI wenv model = widgetTree where
  widgetTree = themeSwitch_ customLightTheme [themeClearBg] $ hstack [
      fileWindow,
      editWindow
    ]

  fileWindow = vstack [
      box (label "Manage proofs") `styleBasic` [padding 10],
      vstack $ map fileItem (model ^. filesInDirectory),
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

  fileItem filePath = box_ [expandContent, onClick (OpenFile filePath)] $ vstack [
      label $ pack filePath
    ]
      `styleHover` [styleIf (not isCurrent) (bgColor hoverColor)]
      `styleBasic` [borderB 1 gray, padding 8, cursorHand, styleIf isCurrent (bgColor isCurrentColor)]
    where
      hoverColor = wenv ^. L.theme . L.userColorMap . at "hoverColor" . non def
      isCurrentColor = wenv ^. L.theme . L.userColorMap . at "selectedFileBg" . non def
      isCurrent = (model ^. currentFile) == Just filePath

  editWindow = vstack [
      fileNavBar (model ^. openFiles),
      proofWindow (model ^. currentFile)
    ]

  fileNavBar filePaths = hscroll (hstack (map boxedLabel filePaths))
    `styleBasic` [bgColor selectedColor, maxHeight 50, minHeight 50, height 50]
    where
      selectedColor = wenv ^. L.theme . L.userColorMap . at "selectedFileBg" . non def
      boxedLabel filePath = box_ [expandContent, onClick (SetCurrentFile filePath)] $ hstack [
          spacer,
          label displayName,
          -- button displayName (SetCurrentFile filePath) `styleBasic` [textColor white, bgColor transparent, paddingV 8, paddingH 16, radius 0, border 0 transparent],
          box (button closeText (CloseFile filePath)
            `styleBasic` [textFont "Symbol_Regular", textSize 24, bgColor transparent, radius 8, border 0 transparent]
            `styleHover` [bgColor $ rgba 255 0 0 0.1])
        ]
          `styleBasic` [borderR 1 gray, styleIf isCurrent (bgColor white)]
          `styleHover` [styleIf (not isCurrent) (bgColor hoverColor)]
          where
            displayName = pack filePath
            closeText = if isEdited then "●" else "⨯"
            isEdited = fromMaybe False (file >>= Just . _isEdited)
            file = getProofFileByPath (model ^. tmpLoadedFiles) filePath
            hoverColor = wenv ^. L.theme . L.userColorMap . at "hoverColor" . non def
            isCurrent = (model ^. currentFile) == Just filePath

  proofWindow Nothing = vstack [] `styleBasic` [expandWidth 1000] -- Don't know how expandWith works, but it works
  proofWindow (Just fileName) = case file of
    Nothing -> label "Filepath not loaded"
    Just file -> keystroke [("Ctrl-s", SaveProof file), ("Ctrl-w", CloseCurrentFile)] $ vstack [
        h1 $ _name file,
        label $ _subname file,
        label $ pack $ _path file,
        spacer,

        hstack [
          label "Conclusion",
          spacer
          -- textField conclusion
        ] `styleBasic` [paddingV 8],
        spacer,

        vscroll $ vstack [
          fst $ proofTreeUI parsedContent,
          spacer,
          hstack_ [childSpacing] [
            button "+ New line" AddLine,
            button "+→ New sub proof" AddSubProof
          ]
        ],
        spacer,

        hstack [
          proofStatusLabel,
          filler,
          button "Save proof" (SaveProof file)
        ]
      ] `styleBasic` [padding 10]
      where parsedContent = _parsedContent file
    where file = getProofFileByPath (model ^. tmpLoadedFiles) fileName

  proofStatusLabel = hstack [
      widgetIf (model ^. proofStatus == Just True) (label "Proof is correct :)" `styleBasic` [textColor lime]),
      widgetIf (model ^. proofStatus == Just False) (label "Proof is not correct!" `styleBasic` [textColor pink]),
      widgetIf (model ^. proofStatus == Nothing) (label "Checking proof..." `styleBasic` [textColor orange])
    ]

  proofTreeUI :: ProofFormula -> (WidgetNode AppModel AppEvent, Integer)
  proofTreeUI formulas = pf formulas 1 []
    where
      pf :: ProofFormula -> Integer -> FormulaPath -> (WidgetNode AppModel AppEvent, Integer)
      pf (MainProof _ formulas) index path = (ui, lastIndex)
        where
          ui = vstack_ [childSpacing] (map fst s)
          lastIndex = if null s then index else snd $ last s
          s = getSubProof formulas path 0 index

      pf (SubProof p) index path = (ui, lastIndex)
        where
          ui = vstack_ [childSpacing] (map fst s) `styleBasic` [border 1 borderColor, borderR 0 transparent, paddingV 8, paddingL 24]
          borderColor = wenv ^. L.theme . L.userColorMap . at "proofBoxColor" . non def
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
            ]
          lastIndex = index + 1

      pf Removed index _ = (label "`Removed` should not appear", index)

      getSubProof p path arrayIndex visualIndex
        | arrayIndex < length p = u : getSubProof p path (arrayIndex + 1) (snd u)
        | otherwise = []
          where u = pf (p !! arrayIndex) visualIndex (path ++ [arrayIndex])


handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
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

  AddLine -> actions
    where
      actions = fromMaybe [] (fileIndex >>= Just . getActions)
      fileIndex = cf >>= getProofFileIndexByPath (model ^. tmpLoadedFiles)
      cf = model ^. currentFile

      getActions fileIndex = [
          Model $ model
            & tmpLoadedFiles . singular (ix fileIndex) . parsedContent %~ addLine
            & tmpLoadedFiles . singular (ix fileIndex) . isEdited .~ True
        ]
      addLine (MainProof conc proof) = MainProof conc (proof ++ [Formula "" ""])
      addLine _ = error "Root must be a `MainProof`"

  AddSubProof -> actions
    where
      actions = fromMaybe [] (fileIndex >>= Just . getActions)
      fileIndex = cf >>= getProofFileIndexByPath (model ^. tmpLoadedFiles)
      cf = model ^. currentFile

      getActions fileIndex = [
          Model $ model
            & tmpLoadedFiles . singular (ix fileIndex) . parsedContent %~ addSubProof
            & tmpLoadedFiles . singular (ix fileIndex) . isEdited .~ True
        ]
      addSubProof (MainProof conc proof) = MainProof conc (proof ++ [newSubProof])
        where newSubProof = SubProof [Formula "" ""]
      addSubProof _ = error "Root must be a `MainProof`"

  RemoveLine path -> actions
    where
      actions = fromMaybe [] (fileIndex >>= Just . getActions)
      fileIndex = cf >>= getProofFileIndexByPath (model ^. tmpLoadedFiles)
      cf = model ^. currentFile

      getActions fileIndex = [
          Model $ model
            & tmpLoadedFiles . singular (ix fileIndex) . parsedContent %~ removeLine path
            & tmpLoadedFiles . singular (ix fileIndex) . isEdited .~ True
        ]

      removeLine removePath = rl removePath []
      rl removePath currentPath (MainProof f p)
        | removePath == currentPath = Removed
        | otherwise = MainProof f (filterRemoved $ zipWith (\p idx -> rl removePath (currentPath ++ [idx]) p) p [0..])
      rl removePath currentPath (SubProof p)
        | removePath == currentPath = Removed
        | otherwise = SubProof $ filterRemoved $ zipWith (\p idx -> rl removePath (currentPath ++ [idx]) p) p [0..]
      rl removePath currentPath f@(Formula _ _)
        | removePath == currentPath = Removed
        | otherwise = f
      rl _ _ p = p
      filterRemoved = filter (/=Removed)

  EditLine path arg newText -> actions
    where
      actions = fromMaybe [] (fileIndex >>= Just . getActions)
      fileIndex = cf >>= getProofFileIndexByPath (model ^. tmpLoadedFiles)
      cf = model ^. currentFile

      getActions fileIndex = [
          Model $ model
            & tmpLoadedFiles . singular (ix fileIndex) . parsedContent %~ editLine path arg newText
            & tmpLoadedFiles . singular (ix fileIndex) . isEdited .~ True
        ]

      editLine editPath arg newText = el editPath arg newText []
      el editPath arg newText currentPath (MainProof f p) = MainProof f (zipWith (\p idx -> el editPath arg newText (currentPath ++ [idx]) p) p [0..])
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
      Producer (\_ -> do
        exists <- doesFileExist filePath
        if exists then return () else writeFile filePath ""
      ),
      Model $ model
        & newFilePopupOpen .~ False
        & newFileName .~ ""
    ]
    where filePath = "./myProofs/" <> unpack fileName <> ".logic"

  SetFilesInDirectory fs -> [ Model $ model & filesInDirectory .~ fs ]

  OpenFile filePath -> [
      Producer (\sendMsg -> do
        pContent <- readFile ("./myProofs/" <> filePath)
        let pName = pack filePath
            pSubname = "subname"
            pContentText = pack pContent
            pParsedContent = parseProofFromFile pContentText
            pIsEdited = False
        sendMsg (OpenFileSuccess $ File filePath pName pSubname pContentText pParsedContent pIsEdited)
      )
    ]

  OpenFileSuccess file -> Model newModel : handleEvent wenv node newModel (SetCurrentFile filePath)
    where
      newModel = model
        & openFiles %~ doOpenFile
        & tmpLoadedFiles %~ createNew file

      doOpenFile currentlyOpenFiles = currentlyOpenFiles ++ [filePath | filePath `notElem` model ^. openFiles]
      createNew newFile oldFiles = filter (\f -> _path newFile /= _path f) oldFiles ++ [newFile]
      filePath = _path file

  CloseCurrentFile -> case model ^. currentFile of
    Nothing -> []
    Just filePath -> handleEvent wenv node model (CloseFile filePath)

  CloseFile filePath -> [ Model finalModel ]
    where
      finalModel = modelWithClosedFile & currentFile .~ (if cf == Just filePath then maybeHead (modelWithClosedFile ^. openFiles) else cf)
      modelWithClosedFile = model & openFiles %~ filter (filePath/=)
      cf = model ^. currentFile

  SaveProof f -> [
      Producer (\sendMsg -> do
        result <- try (writeFile ("./myProofs/" <> fileName) content) :: IO (Either SomeException ())
        case result of
          Left _ -> return ()
          Right _ -> sendMsg (SaveProofSuccess f)
      )
    ]
    where
      content = unpack $ parseProofToFile $ _parsedContent f
      fileName = _path f

  SaveProofSuccess f -> actions
    where
      actions = fromMaybe [] (fileIndex >>= Just . getActions)
      getActions fileIndex = [ Model $ model & tmpLoadedFiles . singular (ix fileIndex) . isEdited .~ False ]
      fileIndex = getProofFileIndexByPath (model ^. tmpLoadedFiles) (_path f)

  SetCurrentFile filePath -> [ Model $ model & currentFile ?~ filePath ]

  BackendResponse (SequentChecked result) -> case result of
    Left err -> [Message "Error" (pack err)]  -- Add type annotation
    Right sequent -> [Model $ model & proofStatus ?~ isProofCorrect sequent]

  BackendResponse (StepChecked result) -> case result of
    Left err -> [Message "Error" (pack err)]  -- Add type annotation
    Right _step -> [Message "Step Status" ("Step is correct" :: Text)]  -- Add type annotation

  -- Log unhandled events instead of crashing
  f -> [ Producer (\_ -> print f) ]

main :: IO ()
main = do
  frontendChan <- newChan
  backendChan <- newChan
  startCommunication frontendChan backendChan
  startApp (model frontendChan backendChan) handleEvent buildUI config
  where
    config = [
      appWindowTitle "Proof Editor",
      appWindowIcon "./assets/images/icon.png",
      appTheme customLightTheme,
      -- appTheme darkTheme,

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
    -- Initial states
    model frontendChan backendChan = AppModel {
      _clickCount = 0,
      _newFileName = "",
      _newFilePopupOpen = False,
      _filesInDirectory = [],
      _tmpLoadedFiles = [],
      _openFiles = [],
      _currentFile = Nothing,

      _frontendChan = frontendChan,
      _backendChan = backendChan,
      _proofStatus = Nothing
    }

-- customTheme :: Theme
-- customTheme = baseTheme lightThemeColors {
--   btnMainBgBasic = rgbHex "#EE9000",
--   btnMainBgHover = rgbHex "#FFB522",
--   btnMainBgFocus = rgbHex "#FFA500",
--   btnMainBgActive = rgbHex "#DD8000",
--   btnMainBgDisabled = rgbHex "#BB8800",
--   btnMainText = rgbHex "000000"
-- }

customLightTheme :: Theme
customLightTheme = baseTheme lightThemeColors {
  btnMainBgBasic = rgbHex "#EE9000",
  btnMainBgHover = rgbHex "#FFB522",
  btnMainBgFocus = rgbHex "#FFA500",
  btnMainBgActive = rgbHex "#DD8000",
  btnMainBgDisabled = rgbHex "#BB8800",
  btnMainText = rgbHex "#FF0000",
  labelText = rgbHex "000000"
}
  & L.userColorMap . at "hoverColor" ?~ rgba 0 0 0 0.05
  & L.userColorMap . at "selectedFileBg" ?~ rgba 0 0 0 0.1
  & L.userColorMap . at "proofBoxColor" ?~ rgbHex "000000"

directoryFilesProducer :: (AppEvent -> IO ()) -> IO ()
directoryFilesProducer sendMsg = do
  allFileNames <- listDirectory "./myProofs"
  -- allFiles <- traverse readProofFile allFileNames
  sendMsg (SetFilesInDirectory allFileNames)

  threadDelay $ 2 * seconds
  directoryFilesProducer sendMsg
    where
      seconds = 1000 * 1000
      -- readProofFile path = do
      --   let pName = pack path
      --       pSubname = "subname"
      --   pContent <- readFile ("./myProofs/" <> path)
      --   let pContentText = pack pContent
      --       parsedContent = parseProofFromFile pContentText
      --       isEdited = False
      --   return $ File path pName pSubname pContentText parsedContent isEdited

h1 :: Text -> WidgetNode s e
h1 t = label t `styleBasic` [ textSize 24, textFont "Bold" ]

iconButton :: Text -> AppEvent -> WidgetNode AppModel AppEvent
iconButton iconIdent action = button iconIdent action
  `styleBasic` [textFont "Remix", textMiddle, textColor orangeRed, bgColor transparent, border 0 transparent]

trashButton :: AppEvent -> WidgetNode AppModel AppEvent
trashButton = iconButton remixDeleteBinFill

getProofFileByPath :: [File] -> FilePath -> Maybe File
getProofFileByPath allFiles filePath = find (\f -> _path f == filePath) allFiles

getProofFileIndexByPath :: [File] -> FilePath -> Maybe Int
getProofFileIndexByPath allFiles filePath = findIndex (\f -> _path f == filePath) allFiles

-- Empty for now
exportProof :: AppModel -> Sequent
exportProof = const $ Seq [] FormBot []

-- evaluateCurrentProof :: AppModel -> IO (Either String Sequent)
-- evaluateCurrentProof model = do
--     let sequent = exportProof model
--     evaluateProofSegment (model ^. frontendChan) (model ^. backendChan) sequent

-- exportProof model = Seq [] (FormPred (Pred (PredId (unpack (model ^. conclusion))) (Params []))) (map toStep (model ^. proofLines))

-- toStep :: ProofLine -> Step
-- toStep line = StepPrem (FormPred (Pred (PredId (unpack (line ^. statement))) (Params [])))

-- Placeholder
parseProofFromFile :: Text -> ProofFormula
parseProofFromFile p = case proof of
  [SubProof p] -> MainProof "" p
  [Removed] -> MainProof "" []
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
    exportProofHelper (MainProof _ p) indent = exportProofHelper (SubProof p) indent
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