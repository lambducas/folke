{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Frontend.Main where

import Frontend.Types

import Monomer
import qualified Monomer.Lens as L
import Monomer.Core.Themes.BaseTheme
import Control.Lens
import Control.Concurrent (threadDelay, Chan, newChan)
import Control.Exception (try, SomeException (SomeException))
import TextShow ( TextShow(showt) )
import System.Directory ( doesFileExist, listDirectory, doesDirectoryExist )
import Data.Text (Text, replace, unpack, pack, intercalate, splitOn)
import Data.List (find, dropWhileEnd, findIndex, sort, groupBy)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, isNothing, catMaybes)
import Data.Default ( Default(def) )
import Data.String (fromString)
import Shared.Messages
import Backend.TypeChecker (isProofCorrect)
import qualified Logic.Abs as Abs
import Frontend.Communication (startCommunication, evaluateProofSegment, evaluateProofString)
import Logic.Par (pSequent, myLexer)
import Control.Monad (filterM, (<$!>))

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

menuBarCategories :: [(Text, [(Text, Text, AppEvent)])]
menuBarCategories = [
    ("File", [
      ("New Proof", "Ctrl+N", OpenCreateProofPopup),
      ("Save Proof", "Ctrl+S", NoEvent)
    ]),
    ("Edit", [
      ("Cut", "Ctrl+X", NoEvent),
      ("Copy", "Ctrl+C", NoEvent),
      ("Paste", "Ctrl+V", NoEvent),

      ("Make Subproof", "Tab", NoEvent),
      ("Undo Subproof", "Shift+Tab", NoEvent),
      ("Insert Line After", "Ctrl+Enter", NoEvent),
      ("Close Subproof", "Ctrl+Enter", NoEvent)
    ]),
    ("View", [
      ("Open Settings", "Ctrl+Shift+P", NoEvent),
      ("Toggle Theme", "", SwitchTheme)
    ]),
    ("Help", [
      ("Open Guide", "", NoEvent)
    ]),
    ("Settings", [
      ("Settings", "", OpenFile_ "Settings.json" "")
    ])
  ]

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI _wenv model = widgetTree where
  selTheme = model ^. selectedTheme
  popupBackground = selTheme ^. L.userColorMap . at "popupBackground" . non def
  backgroundColor = selTheme ^. L.userColorMap . at "backgroundColor" . non def
  selectedColor = selTheme ^. L.userColorMap . at "selectedFileBg" . non def
  dividerColor = selTheme ^. L.userColorMap . at "dividerColor" . non def
  hoverColor = selTheme ^. L.userColorMap . at "hoverColor" . non def
  proofBoxColor = selTheme ^. L.userColorMap . at "proofBoxColor" . non def

  widgetTree = themeSwitch_ selTheme [themeClearBg] $ vstack [
      vstack [
        menuBar,
        mainContent
      ],
      popup_ confirmDeletePopup [popupAlignToWindow, popupDisableClose, alignCenter, alignMiddle] (vstack_ [childSpacing] [
        h1 "Close without saving?",
        ourLabel "Are you sure you want to close",
        ourLabel (showt $ model ^. confirmDeleteTarget),
        ourLabel "without saving. All changes will be lost!",
        spacer,
        hstack_ [childSpacing] [
          button "Close anyway" (maybe NoEvent CloseFileSuccess (model ^. confirmDeleteTarget)),
          toggleButton "Cancel" confirmDeletePopup
        ]
      ] `styleBasic` [bgColor popupBackground, border 1 dividerColor, padding 20])
    ]

  menuBar = hstack (zipWith menuBarButton menuBarCategories [0..])
    `styleBasic` [borderB 1 dividerColor, padding 5]
    where
      menuBarButton (name, actions) idx = vstack [
          box_ [onClick (SetOpenMenuBarItem (Just idx))] (
            label name
              `styleBasic` [textSize 14, radius 4, paddingV 5, paddingH 10]
              `styleHover` [bgColor selectedColor]
          ),
          popupV (Just idx == model ^. openMenuBarItem) (\s -> if s then SetOpenMenuBarItem (Just idx) else SetOpenMenuBarItem Nothing)
            (vstack (map dropdownButton actions)
              `styleBasic` [width 300, bgColor popupBackground, border 1 dividerColor, padding 4, radius 4])
        ]

      dropdownButton (name, keybind, action) = box_ [onClick action, onClick (SetOpenMenuBarItem Nothing), expandContent] $ hstack [
          ourLabel name `styleBasic` [textSize 14],
          filler,
          ourLabel keybind `styleBasic` [textSize 14]
        ]
          `styleBasic` [radius 4, paddingV 10, paddingH 20, cursorHand]
          `styleHover` [bgColor hoverColor]

  mainContent = hstack [
      fileWindow,
      editWindow
    ] `styleBasic` [expandHeight 1000]

  fileWindow = vstack [
      box_ [expandContent] (hstack [
          ourLabel "Manage proofs" `styleBasic` [padding 10, textFont "Bold"],
          filler,
          box (button "+" OpenCreateProofPopup) `styleBasic` [bgColor transparent, padding 4],

          popup newFilePopupOpen (vstack [
            ourLabel "Enter the name of your proof",
            spacer,
            textField_ newFileName [placeholder "my_proof"],
            spacer,
            let cep = (CreateEmptyProof $ model ^. newFileName) in
              keystroke [("Enter", cep)] $ button "Create proof" cep
          ] `styleBasic` [bgColor popupBackground, padding 10])
        ]) `styleBasic` [borderB 1 dividerColor],
      vscroll $ fileTreeUI parts 1
    ] `styleBasic` [ width 250, borderR 1 dividerColor ]
    where
      parts = map (\f -> (splitOn "/" (pack f), f)) files
      files = sort (model ^. filesInDirectory)

      fileTreeUI parts indent = vstack [
          vstack $ map (\f -> fileItem indent (fst f) (snd f)) partFile,
          vstack $ map folder groups
        ]
        where
          -- parts = map (\f -> (splitOn "/" (pack f), f)) files
          partFile = map (\f -> ((head . fst) f, snd f)) (filter (\i -> length (fst i) == 1) parts)
          partFolder = filter (\i -> length (fst i) > 1) parts
          groups = groupBy (\a b -> head (fst a) == head (fst b)) partFolder
          
          folder seqs = vstack [
              hstack [
                ourLabel ((head . fst . head) seqs),
                iconLabel remixFolder5Line `styleBasic` [paddingL 8]
              ] `styleBasic` [paddingL (16 * indent), paddingV 8],
              fileTreeUI newParts (indent + 1)
            ]
            where
              newParts = map (\f -> ((tail . fst) f, snd f)) seqs
              -- newParts = map (\f -> (splitOn "/" (pack f), f)) newFiles
              -- newFiles = map (unpack . intercalate "/" . tail . fst) seqs

  fileItem indent text filePath = box_ [expandContent, onClick (OpenFile filePath)] $ vstack [
      label_ text [ellipsis]
    ]
      `styleHover` [styleIf (not isCurrent) (bgColor hoverColor)]
      `styleBasic` [borderB 1 dividerColor, paddingL (16 * indent), paddingR 16, paddingV 8, cursorHand, styleIf isCurrent (bgColor selectedColor)]
    where
      isCurrent = (model ^. currentFile) == Just filePath

  editWindow = vstack [
      fileNavBar (model ^. openFiles),
      proofWindow (model ^. currentFile)
    ]

  fileNavBar filePaths = hscroll (hstack (map boxedLabel filePaths))
    `styleBasic` [bgColor selectedColor, maxHeight 50, minHeight 50, height 50]
    where
      boxedLabel filePath = box_ [expandContent, onClick (SetCurrentFile filePath)] $ hstack [
          spacer,
          ourLabel displayName,
          -- button displayName (SetCurrentFile filePath) `styleBasic` [textColor white, bgColor transparent, paddingV 8, paddingH 16, radius 0, border 0 transparent],
          box_ [onClick (CloseFile filePath)] (ourLabel closeText
            `styleBasic` [textFont "Symbol_Regular", textSize 24, radius 8, padding 4]
            `styleHover` [bgColor hoverColor]) `styleBasic` [padding 4]
        ]
          `styleBasic` [borderR 1 dividerColor, styleIf isCurrent (bgColor backgroundColor), cursorHand]
          `styleHover` [styleIf (not isCurrent) (bgColor hoverColor)]
          where
            displayName = pack filePath
            closeText = if isEdited then "●" else "⨯"
            isEdited = fromMaybe False (file >>= Just . _isEdited)
            file = getProofFileByPath (model ^. tmpLoadedFiles) filePath
            isCurrent = (model ^. currentFile) == Just filePath

  illustThickness fontThicknessess = vstack [label "This is how thick I am" `styleBasic` [textFont $ fromString fontThicknessess]]
  proofWindow Nothing = vstack [] `styleBasic` [expandWidth 1000] -- Don't know how expandWith works, but it works
  proofWindow (Just "Settings.json") = hstack [
      vstack [
          label "Choose font" `styleBasic` [textFont $ fromString $ model ^. normalFont],
          textDropdown_ selectNormalFont [
            ["Regular","Medium","Bold"], 
            ["Dyslexic"],
            ["Roboto_Regular","Roboto_Medium","Roboto_Bold"], 
            ["Comic_Sans_Regular", "Comic_Sans_Thin", "Comic_Sans_Medium", "Comic_Sans_Bold"]
            ] fontListToText [],
          label "This is how I look" `styleBasic` [textFont $ fromString $ head $ model ^. selectNormalFont],
          button "Set font" UpdateFont,
          spacer,
          label "Set thickness" `styleBasic` [textFont $ fromString $ model ^. normalFont],
          textDropdown_ normalFont (model ^. selectNormalFont) pack [],
          vstack $ map illustThickness (model ^. selectNormalFont),
          spacer,
          spacer,
          label "Set symbolic font for proofs" `styleBasic` [textFont $ fromString $ model ^. logicFont],
          textDropdown_ logicFont ["Symbol_Regular","Symbol_Medium","Symbol_Bold"] pack []
        ]
      ]
  proofWindow (Just fileName) = case file of
    Nothing -> label "Filepath not loaded"
    Just file -> keystroke [("Ctrl-s", SaveProof file), ("Ctrl-w", CloseCurrentFile)] $ vstack [
        h1 $ pack $ _path file,
        spacer,
        label prettySequent `styleBasic` [textFont $ fromString $ model ^. logicFont],
        spacer, spacer, spacer, spacer,

        scroll_ [wheelRate 50] $ proofTreeUI parsedSequent,
        spacer,

        hstack [
          proofStatusLabel,
          filler,
          button "Save proof" (SaveProof file),
          spacer,
          button "Check proof" (CheckProof file)
        ]
      ] `styleBasic` [padding 10]
      where
        prettySequent = intercalate ", " (_premises parsedSequent) <> " ⊢ " <> _conclusion parsedSequent
        parsedSequent = _parsedSequent file
    where file = getProofFileByPath (model ^. tmpLoadedFiles) fileName

  proofStatusLabel = case model ^. proofStatus of
    Nothing -> ourLabel "Checking proof..." `styleBasic` [textColor orange]
    Just (Left error) -> ourLabel ("Proof is incorrect: " <> pack error) `styleBasic` [textColor red]
    Just (Right _) -> ourLabel "Proof is correct :)" `styleBasic` [textColor lime]

  proofTreeUI :: FESequent -> WidgetNode AppModel AppEvent
  proofTreeUI sequent = vstack [
      vstack_ [childSpacing] [
        ourLabel "Premises" `styleBasic` [textFont $ fromString $ last $ model ^. selectNormalFont],
        vstack_ [childSpacing] $ zipWith premiseLine (_premises sequent) [0..],
        widgetIf (null $ _premises sequent) (ourLabel "No premises")
      ],
      spacer,
      hstack [button "+ Premise" AddPremise],
      spacer, spacer,

      ourLabel "Conclusion" `styleBasic` [textFont $ fromString $ last $ model ^. selectNormalFont],
      spacer,
      textFieldV_ (replaceSpecialSymbols (_conclusion sequent)) EditConclusion [placeholder "Enter conclusion here"],
      spacer, spacer,

      ourLabel "Proof" `styleBasic` [textFont $ fromString $ last $ model ^. selectNormalFont],
      spacer,
      tree,

      -- spacer,
      -- hstack_ [childSpacing] [
      --   button "+ New line" AddLine,
      --   button "+→ New sub proof" AddSubProof
      -- ]

      -- Hack so last proof line can scroll all the way to the top
      box (ourLabel "") `styleBasic` [height 1000]
    ]
    where
      premiseLine premise idx = hstack [
          textFieldV_ (replaceSpecialSymbols premise) (EditPremise idx) [placeholder "Enter premise"],
          spacer,
          tooltip "Remove line" $ trashButton (RemovePremise idx),
          spacer
        ]

      tree = ui
        where
          ui = vstack_ [childSpacing] (map fst s)
          s = getSubProof (_steps sequent) [] 0 1

      pf :: FEStep -> Integer -> FormulaPath -> (WidgetNode AppModel AppEvent, Integer)
      pf (SubProof p) index path = (ui, lastIndex)
        where
          ui = vstack_ [childSpacing] (map fst s) `styleBasic` [border 1 proofBoxColor, borderR 0 transparent, paddingV 8, paddingL 24]
          lastIndex = if null s then index else snd $ last s
          s = getSubProof p path 0 index

      pf (Line statement rule) index path = (ui, lastIndex)
        where
          ui = hstack [
              hstack [
                label (showt index <> ".") `styleBasic` [textFont $ fromString $ model ^. logicFont],
                spacer,

                firstKeystroke [
                  ("Up", FocusOnKey $ WidgetKey (showt (index - 1) <> ".statement"), prevIndexExists),
                  ("Down", FocusOnKey $ WidgetKey (showt (index + 1) <> ".statement"), nextIndexExists),
                  -- ("Right", FocusOnKey $ WidgetKey (showt index <> ".rule"), True),

                  ("Tab", SwitchLineToSubProof path, True),
                  ("Shift-Tab", SwitchSubProofToLine pathToParentSubProof, True),
                  ("Delete", RemoveLine path, True),
                  ("Ctrl-Enter", InsertLineAfter path, not isLastLine),
                  ("Ctrl-Enter", InsertLineAfter pathToParentSubProof, isLastLine),
                  ("Enter", NextFocus 1, True)
                ] $
                textFieldV (replaceSpecialSymbols statement) (EditLine path 0)
                  `styleBasic` [textFont $ fromString $ model ^. logicFont]
                  `nodeKey` showt index <> ".statement",

                spacer,

                firstKeystroke [
                  ("Up", FocusOnKey $ WidgetKey (showt (index - 1) <> ".rule"), prevIndexExists),
                  ("Down", FocusOnKey $ WidgetKey (showt (index + 1) <> ".rule"), nextIndexExists),
                  -- ("Left", FocusOnKey $ WidgetKey (showt index <> ".statement"), True),

                  ("Tab", SwitchLineToSubProof path, True),
                  ("Shift-Tab", SwitchSubProofToLine pathToParentSubProof, True),
                  ("Delete", RemoveLine path, True),
                  ("Ctrl-Enter", InsertLineAfter pathToParentSubProof, isLastLine),
                  ("Enter", InsertLineAfter path, True)
                ] $
                textFieldV (replaceSpecialSymbols rule) (EditLine path 1)
                  `styleBasic` [textFont "Symbol_Regular", width 175]
                  `nodeKey` showt index <> ".rule"
              ]
                `nodeKey` showt index,

              spacer,

              b
            ]
          b = box $ hstack_ [childSpacing] [
                tooltip "Remove line" $ trashButton (RemoveLine path),

                tooltip "Convert line to subproof" $ button "→☐" (SwitchLineToSubProof path),
                widgetIf isSubProofSingleton $
                  tooltip "Undo subproof" (button "☒" (SwitchSubProofToLine pathToParentSubProof)),
                tooltip "Add line after" $ button "↓+" (InsertLineAfter path),
                -- button "|[]+" (InsertSubProofAfter path),
                widgetIf (isLastLine && nextIndexExists) $
                  tooltip "Close subproof" (button "⏎" (InsertLineAfter pathToParentSubProof))
                -- widgetIf isLastLine (button "/[]+" (InsertSubProofAfter pathToParentSubProof))
              ] `styleBasic` [width 300]

          pathToParentSubProof = init path
          lastIndex = index + 1
          prevIndexExists = index > 1
          nextIndexExists = not (isLastLine && length path == 1)
          isSubProofSingleton = length path /= 1 && isSingleton (evalPath sequent pathToParentSubProof)
          isSingleton (SubProof p) = length p == 1
          isSingleton _ = False
          isLastLine = case evalPath sequent pathToParentSubProof of
            SubProof p -> length p == last path + 1
            _ -> False

      getSubProof p path arrayIndex visualIndex
        | arrayIndex < length p = u : getSubProof p path (arrayIndex + 1) (snd u)
        | otherwise = []
          where u = pf (p !! arrayIndex) visualIndex (path ++ [arrayIndex])

  h1 :: Text -> WidgetNode s e
  h1 t = label t `styleBasic` [ textSize 24, textFont $ fromString $ last $ model ^. selectNormalFont ]

  ourLabel :: Text -> WidgetNode s e
  ourLabel t = label t `styleBasic` [ textFont $ fromString $ model ^. normalFont ]


handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  NoEvent -> []

  AppInit -> [
      Producer directoryFilesProducer,
      Task $ do
        frontendChan <- newChan
        backendChan <- newChan
        answer <- startCommunication frontendChan backendChan
        return $ BackendResponse answer
    ]

  SetOpenMenuBarItem s -> [ Model $ model & openMenuBarItem .~ s ]

  FocusOnKey key -> [ SetFocusOnKey key ]

  NextFocus n -> replicate n (MoveFocusFromKey Nothing FocusFwd)

  AddPremise -> applyOnCurrentProof model addPremiseToProof

  RemovePremise idx -> applyOnCurrentProof model (removePremiseFromProof idx)

  EditPremise idx newText -> applyOnCurrentProof model (editPremisesInProof idx newText)

  EditConclusion newText -> applyOnCurrentProof model (editConclusionInProof newText)

  SwitchLineToSubProof path -> applyOnCurrentProof model switch ++ focusAction
    where
      switch = replaceInProof path (\oldLine -> SubProof [oldLine])

      focusAction = fromMaybe [] maybeFocusAction
      maybeFocusAction = (getCurrentSequent model >>= \f -> Just $ pathToLineNumber f path) >>= getFocusAction
      getFocusAction l = Just [ SetFocusOnKey (WidgetKey $ showt l <> ".statement"), MoveFocusFromKey Nothing FocusBwd ]

  SwitchSubProofToLine path -> applyOnCurrentProof model switch ++ focusAction
    where
      switch p = if not $ isSingleton $ evalPath p path then p else replaceInProof path (\oldLine -> case oldLine of
        SubProof p -> head p
        _ -> error ""
        ) p
      isSingleton (SubProof p) = length p == 1
      isSingleton _ = False

      focusAction = fromMaybe [] maybeFocusAction
      maybeFocusAction = (getCurrentSequent model >>= \f -> Just $ pathToLineNumber f path) >>= getFocusAction
      getFocusAction l = Just [ SetFocusOnKey (WidgetKey $ showt l <> ".statement"), MoveFocusFromKey Nothing FocusFwd ]

  InsertLineAfter path -> applyOnCurrentProof model insertLine ++ focusAction
    where
      focusAction = fromMaybe [] maybeFocusAction
      maybeFocusAction = (getCurrentSequent model >>= \f -> Just $ pathToLineNumber f path) >>= getFocusAction
      getFocusAction l = Just [ SetFocusOnKey (WidgetKey $ showt (l + 1) <> ".statement") ]
      insertLine = insertAfterProof path (Line "" "")

  InsertSubProofAfter path -> applyOnCurrentProof model insertSubProof
    where insertSubProof = insertAfterProof path (SubProof [Line "" ""])

  AddLine -> applyOnCurrentProof model addLine
    where
      addLine sequent = FESequent premises conclusion steps
        where
          premises = _premises sequent
          conclusion = _conclusion sequent
          steps = _steps sequent ++ [Line "" ""]

  AddSubProof -> applyOnCurrentProof model addSubProof
    where
      addSubProof sequent = FESequent premises conclusion steps
        where
          premises = _premises sequent
          conclusion = _conclusion sequent
          steps = _steps sequent ++ [SubProof [Line "" ""]]

  RemoveLine path -> applyOnCurrentProof model removeLine
    where removeLine = removeFromProof path

  EditLine path arg newText -> applyOnCurrentProof model editLine
    where editLine = editFormulaInProof path arg newText

  OpenCreateProofPopup -> [ Model $ model & newFilePopupOpen .~ True ]

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

  OpenFile_ filePath folderPath -> [
      Producer (\sendMsg -> do
        pContent <- readFile (folderPath <> filePath)
        let pContentText = pack pContent
            -- pParsedContent = parseProofFromFile pContentText
            pIsEdited = False
        case pSequent (myLexer pContent) of
          Left _err -> sendMsg (OpenFileSuccess $ File filePath pContentText (FESequent [] "" [Line "Invalid" ""]) pIsEdited)
          Right seq_t -> sendMsg (OpenFileSuccess $ File filePath pContentText pParsedContent pIsEdited)
            where pParsedContent = convertSeq seq_t
      )
    ]
    where
      convertSeq (Abs.Seq premises conclusion proof) = FESequent (map convertForm premises) (convertForm conclusion) (convertProof proof)

      convertForm Abs.FormBot = "bot"
      -- convertForm (Abs.FormEq a b) = (convertForm a) <> " = " <> (convertForm b)
      convertForm (Abs.FormPred (Abs.Pred (Abs.Ident i) _params)) = pack i
      -- convertForm (Abs.FormAll a b) = "#"
      -- convertForm (Abs.FormSome a b) = "#"
      convertForm (Abs.FormNot a) = "!" <> convertForm a
      convertForm (Abs.FormAnd a b) = "(" <> convertForm a <> " & " <> convertForm b <> ")"
      convertForm (Abs.FormOr a b) = "(" <> convertForm a <> " | " <> convertForm b <> ")"
      convertForm (Abs.FormIf a b) = "(" <> convertForm a <> " -> " <> convertForm b <> ")"
      convertForm s = error (show s)

      convertProof (Abs.Proof proofElems) = map convertProofElem proofElems
      convertProofElem (Abs.ProofElem _labels step) = convertStep step

      convertStep (Abs.StepPrem form) = Line (convertForm form) "prem"
      convertStep (Abs.StepAssume form) = Line (convertForm form) "assume"
      convertStep (Abs.StepProof proof) = SubProof (convertProof proof)
      convertStep (Abs.StepForm (Abs.Ident i) args form) = Line (convertForm form) (pack i <> " [" <> intercalate ", " (map convertArg args) <> "]")
      convertStep s = error (show s)

      convertArg (Abs.ArgLine i) = showt i
      convertArg (Abs.ArgRange a b) = showt a <> "-" <> showt b
  
  OpenFile filePath -> handleEvent wenv node model (OpenFile_ filePath "./myProofs/")

  OpenFileSuccess file -> Model newModel : handleEvent wenv node newModel (SetCurrentFile filePath)
    where
      newModel = model
        & openFiles %~ doOpenFile
        & tmpLoadedFiles %~ createNew file

      doOpenFile currentlyOpenFiles = currentlyOpenFiles ++ [filePath | filePath `notElem` model ^. openFiles]
      createNew newFile oldFiles = if _path newFile `elem` (model ^. openFiles) then
          oldFiles else
          filter (\f -> _path newFile /= _path f) oldFiles ++ [newFile]
      filePath = _path file

  CloseCurrentFile -> case model ^. currentFile of
    Nothing -> []
    Just filePath -> handleEvent wenv node model (CloseFile filePath)

  CloseFile filePath -> case file of
    Just file -> if _isEdited file then [
        Model $ model
          & confirmDeletePopup .~ True
          & confirmDeleteTarget .~ Just filePath
      ] else handleEvent wenv node model (CloseFileSuccess filePath)
    Nothing -> []
    where file = getProofFileByPath (model ^. tmpLoadedFiles) filePath

  CloseFileSuccess filePath -> [ Model finalModel ]
    where
      finalModel = modelWithClosedFile
        & currentFile .~ (if cf == Just filePath then maybeHead (modelWithClosedFile ^. openFiles) else cf)
      modelWithClosedFile = model
        & openFiles %~ filter (filePath/=)
        & confirmDeleteTarget .~ Nothing
        & confirmDeletePopup .~ False
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
      content = unpack $ parseProofForBackend $ _parsedSequent f
      -- content = unpack $ parseProofToFile $ _parsedSequent f
      fileName = _path f

  SaveProofSuccess f -> actions
    where
      actions = fromMaybe [] (fileIndex >>= Just . getActions)
      getActions fileIndex = [ Model $ model & tmpLoadedFiles . singular (ix fileIndex) . isEdited .~ False ]
      fileIndex = getProofFileIndexByPath (model ^. tmpLoadedFiles) (_path f)

  SetCurrentFile filePath -> [
      Model $ model
        & currentFile ?~ filePath
        & proofStatus .~ Nothing
    ]

  SwitchTheme -> [
      Model $ model & selectedTheme %~ switchTheme
    ]
    where
      switchTheme oldTheme
        | oldTheme == customLightTheme = customDarkTheme
        | oldTheme == customDarkTheme = customLightTheme
        | otherwise = customLightTheme

  UpdateFont -> [Model $ model & normalFont .~ head (model ^. selectNormalFont)]
  -- Backend events
  CheckProof file -> [
      Model $ model & proofStatus .~ Nothing,
      Producer (evaluateCurrentProof model file)
    ]

  BackendResponse (StringSequentChecked result) -> case result of
    Left error -> [ Model $ model & proofStatus ?~ Left error ]
    Right _ -> [ Model $ model & proofStatus ?~ Right () ]

  BackendResponse (SequentChecked result) -> case result of
    Left error -> [ Model $ model & proofStatus ?~ Left error ]
    Right _ -> [ Model $ model & proofStatus ?~ Right () ]

  BackendResponse (OtherBackendMessage message) -> [ Producer (\_ -> print $ "From backend: " ++ message) ]

  -- BackendResponse (SequentChecked result) -> case result of
  --   Left err -> [Message "Error" (pack err)]  -- Add type annotation
  --   Right sequent -> [Model $ model & proofStatus ?~ isProofCorrect sequent]

  -- BackendResponse (StepChecked result) -> case result of
  --   Left err -> [Message "Error" (pack err)]  -- Add type annotation
  --   Right _step -> [Message "Step Status" ("Step is correct" :: Text)]  -- Add type annotation

  -- Log unhandled events instead of crashing
  f -> [ Producer (\_ -> print f) ]

main :: IO ()
main = do
  frontendChan <- newChan
  backendChan <- newChan
  _ <- startCommunication frontendChan backendChan
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

      appFontDef "Dyslexic" "assets/fonts/Dyslexic/open-dyslexic.ttf",

      appFontDef "Roboto_Regular" "./assets/fonts/Roboto/Roboto-Regular.ttf",
      appFontDef "Roboto_Medium" "./assets/fonts/Roboto/Roboto-Medium.ttf",
      appFontDef "Roboto_Bold" "./assets/fonts/Roboto/Roboto-Bold.ttf",

      appFontDef "Comic_Sans_Medium" "assets/fonts/ldfcomicsans-font/Ldfcomicsans-jj7l.ttf",
      appFontDef "Comic_Sans_Bold" "assets/fonts/ldfcomicsans-font/Ldfcomicsansbold-zgma.ttf",
      appFontDef "Comic_Sans_Regular" "assets/fonts/ldfcomicsans-font/Ldfcomicsanshairline-5PmL.ttf",
      appFontDef "Comic_Sans_Thin" "assets/fonts/ldfcomicsans-font/Ldfcomicsanslight-6dZo.ttf",

      appFontDef "Symbol_Regular" "./assets/fonts/JuliaMono/JuliaMono-Regular.ttf",
      appFontDef "Symbol_Medium" "./assets/fonts/JuliaMono/JuliaMono-Medium.ttf",
      appFontDef "Symbol_Bold" "./assets/fonts/JuliaMono/JuliaMono-Bold.ttf",

      appFontDef "Remix" "./assets/fonts/remixicon.ttf",

      appInitEvent AppInit,
      appModelFingerprint show
      ]
    -- Initial states
    model frontendChan backendChan = AppModel {
      _openMenuBarItem = Nothing,

      _newFileName = "",
      _newFilePopupOpen = False,
      _filesInDirectory = [],
      _tmpLoadedFiles = [],
      _openFiles = [],
      _currentFile = Nothing,
      _confirmDeletePopup = False,
      _confirmDeleteTarget = Nothing,

      _frontendChan = frontendChan,
      _backendChan = backendChan,
      _proofStatus = Nothing,

      _selectedTheme = customDarkTheme,

      _selectNormalFont = ["Regular","Medium","Bold"],
      _normalFont = "Regular",
      _logicFont = "Symbol_Regular"
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
  clearColor = rgb 255 255 255,
  btnMainBgBasic = rgbHex "#EE9000",
  btnMainBgHover = rgbHex "#FFB522",
  btnMainBgFocus = rgbHex "#FFA500",
  btnMainBgActive = rgbHex "#DD8000",
  btnMainBgDisabled = rgbHex "#BB8800",
  btnMainText = rgbHex "#FF0000",
  labelText = rgbHex "000000"
}
  & L.userColorMap . at "popupBackground" ?~ rgb 230 230 230
  & L.userColorMap . at "backgroundColor" ?~ rgb 255 255 255
  & L.userColorMap . at "hoverColor" ?~ rgba 0 0 0 0.05
  & L.userColorMap . at "selectedFileBg" ?~ rgba 0 0 0 0.1
  & L.userColorMap . at "dividerColor" ?~ rgba 0 0 0 0.1
  & L.userColorMap . at "proofBoxColor" ?~ rgba 0 0 0 0.3

customDarkTheme :: Theme
customDarkTheme = baseTheme darkThemeColors {
  clearColor = rgb 30 30 30,

  inputBgBasic = rgb 50 50 50,

  btnBgBasic = rgb 50 50 50,
  btnBgHover = rgb 70 70 70,
  btnBgFocus = rgb 60 60 60,
  btnBgActive = rgb 90 90 90,
  btnBgDisabled = rgb 40 40 40,
  btnTextDisabled = rgb 70 70 70,
  btnText = rgbHex "#FFFFFF",

  btnMainBgBasic = rgbHex "#EE9000",
  btnMainBgHover = rgbHex "#0000FF",
  btnMainBgFocus = rgbHex "#FFA500",
  btnMainBgActive = rgbHex "#DD8000",
  btnMainBgDisabled = rgbHex "#BB8800",
  btnMainText = rgbHex "#FF0000",
  labelText = rgbHex "#FFFFFF"
}
  & L.userColorMap . at "popupBackground" ?~ rgb 50 50 50
  & L.userColorMap . at "backgroundColor" ?~ rgb 30 30 30
  & L.userColorMap . at "hoverColor" ?~ rgba 255 255 255 0.05
  & L.userColorMap . at "selectedFileBg" ?~ rgba 255 255 255 0.1
  & L.userColorMap . at "dividerColor" ?~ rgba 255 255 255 0.1
  & L.userColorMap . at "proofBoxColor" ?~ rgba 255 255 255 0.3

directoryFilesProducer :: (AppEvent -> IO ()) -> IO ()
directoryFilesProducer sendMsg = do
  let dir = "./myProofs"
  allFileNames <- fmap (map (drop (length dir + 1))) (listDirectoryRecursive dir)
  -- onlyFiles <- filterM (doesFileExist . (dir++)) allFileNames
  -- onlyDirs <- filterM (doesDirectoryExist . (dir++)) allFileNames
  sendMsg (SetFilesInDirectory allFileNames)

  threadDelay $ 2 * seconds
  directoryFilesProducer sendMsg
    where seconds = 1000 * 1000

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive directory = do
  content <- listDirectory directory
  onlyFiles <- filterM doesFileExist (map appendTop content)
  onlyDirs <- filterM doesDirectoryExist (map appendTop content)
  extraFiles <- fmap concat (mapM listDirectoryRecursive onlyDirs)
  return $ onlyFiles ++ extraFiles
    where
      appendTop :: FilePath -> FilePath
      appendTop = ((directory ++ "/") ++)

iconLabel :: Text -> WidgetNode s e
iconLabel icon = label icon `styleBasic` [textFont "Remix", textBottom]

iconButton :: Text -> AppEvent -> WidgetNode AppModel AppEvent
iconButton iconIdent action = button iconIdent action
  `styleBasic` [textFont "Remix", textMiddle, textColor orangeRed, bgColor transparent, border 0 transparent]

trashButton :: AppEvent -> WidgetNode AppModel AppEvent
trashButton = iconButton remixDeleteBinFill

getProofFileByPath :: [File] -> FilePath -> Maybe File
getProofFileByPath allFiles filePath = find (\f -> _path f == filePath) allFiles

getProofFileIndexByPath :: [File] -> FilePath -> Maybe Int
getProofFileIndexByPath allFiles filePath = findIndex (\f -> _path f == filePath) allFiles

evaluateCurrentProof :: AppModel -> File -> (AppEvent -> IO b) -> IO b
evaluateCurrentProof model file sendMsg = do
    -- let sequent = exportProof file
    -- answer <- evaluateProofSegment (model ^. frontendChan) (model ^. backendChan) sequent
    -- sendMsg (BackendResponse answer)

    let text = unpack $ parseProofForBackend (_parsedSequent file)
    putStrLn text
    answer <- evaluateProofString (model ^. frontendChan) (model ^. backendChan) text
    sendMsg (BackendResponse answer)

-- Empty for now
-- exportProof :: File -> Sequent
-- exportProof file = Seq [] FormBot []
--   where
--     _feSequent = _parsedSequent file
-- -- exportProof model = Seq [] (FormPred (Pred (PredId (unpack (model ^. conclusion))) (Params []))) (map toStep (model ^. proofLines))
-- --   where
-- --     toStep :: ProofLine -> Step
-- --     toStep line = StepPrem (FormPred (Pred (PredId (unpack (line ^. statement))) (Params [])))

-- Placeholder
parseProofFromFile :: Text -> FESequent
parseProofFromFile p = case proof of
  [SubProof p] -> FESequent premises conclusion p
  _ -> error "Invalid proof from `parseText`"
  where
    premises = filter (/="") (map trimText (splitOn "," (pack $ slice 0 premiseEnd text)))
    conclusion = trimText (pack (slice (premiseEnd + 1) conclusionEnd text))

    premiseEnd = snd $ fromMaybe (error "Empty premise") (gotoNextChar text (-1) [';'])
    conclusionEnd = snd $ fromMaybe (error "Empty conclusion") (gotoNextChar text premiseEnd [';'])
    proofStart = snd $ fromMaybe (error "No proof") (gotoNextChar text conclusionEnd ['{'])

    proof = [parseText text proofStart []]
    -- proof = [parseText (trim $ drop (conclusionEnd + 1) text) 0 []]
    -- proof = [parseText text 0 []]
    text = unpack p

    parseText :: String -> Int -> [FEStep] -> FEStep
    parseText text ptr formulas = case nextSpecialChar of
      Just (char, idx) -> case char of
        ';' -> parseText text idx (formulas ++ [parseFormula $ pack $ slice (ptr + 1) idx text])
        '{' -> parseText text (findClosingBracket text 0 idx 0 + 1) (formulas ++ [parseText (slice (idx + 1) (findClosingBracket text 0 idx 0 + 1) text) 0 []])
        '}' -> SubProof formulas
        _ -> error "Invalid special char"
      Nothing -> SubProof [Line "" ""] -- Return garbage instead of crashing
      where
        nextSpecialChar = gotoNextChar text ptr ['{', '}', ';']

    parseFormula :: Text -> FEStep
    parseFormula text = Line statement rule
      where
        statement = trimText $ head parts
        rule = trimText $ parts !! 1
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
      | idx >= length text - 1 = idx -- error "No closing bracket found"
      | otherwise = case char of
      '{' -> findClosingBracket text nestedLevel (idx + 1) (cnl + 1)
      '}' -> if nestedLevel == cnl then idx + 1 else findClosingBracket text nestedLevel (idx + 1) (cnl - 1)
      _ -> findClosingBracket text nestedLevel (idx + 1) cnl
      where char = text !! (idx + 1)

parseProofForBackend :: FESequent -> Text
parseProofForBackend sequent = premises <> " |- " <> conclusion <> " " <> exportProofHelper 0 [] (SubProof (_steps sequent))
  where
    premises = replaceSpecialSymbolsInverse $ intercalate "," (_premises sequent)
    conclusion = replaceSpecialSymbolsInverse $ _conclusion sequent

    exportProofHelper :: Int -> FormulaPath -> FEStep -> Text
    exportProofHelper indent path (SubProof p) = label <> tabs indent <> "{\n" <> intercalate "\n" (zipWith (\p idx -> exportProofHelper (indent + 1) (path ++ [idx]) p) p [0..]) <> "\n" <> tabs indent <> "}"
      where label = if null p || null path then "" else showt (pathToLineNumber sequent (path ++ [0])) <> "-" <> showt (pathToLineNumber sequent (path ++ [length p - 1])) <> ":"
    exportProofHelper indent path (Line statement rule) = label <> tabs indent <> nRule <> " " <> nStatement <> ";"
      where
        nRule = replaceSpecialSymbolsInverse rule
        nStatement = replaceSpecialSymbolsInverse statement
        label = showt (pathToLineNumber sequent path) <> ":"

    tabs :: Int -> Text
    tabs n = pack $ replicate n '\t'

-- Placeholder
parseProofToFile :: FESequent -> Text
parseProofToFile sequent = "\n" <> premises <> ";\n" <> conclusion <> ";\n" <> exportProofHelper (SubProof (_steps sequent)) 0
  where
    premises = replaceSpecialSymbolsInverse $ intercalate "," (_premises sequent)
    conclusion = replaceSpecialSymbolsInverse $ _conclusion sequent

    exportProofHelper :: FEStep -> Int -> Text
    exportProofHelper (SubProof p) indent = tabs indent <> "{\n" <> intercalate "\n" (map (`exportProofHelper` (indent + 1)) p) <> "\n" <> tabs indent <> "}"
    exportProofHelper (Line statement rule) indent = tabs indent <> statement <> " : " <> rule <> ";"

    tabs :: Int -> Text
    tabs n = pack $ replicate n '\t'

replaceFromLookup :: Text -> SymbolDict -> Text
replaceFromLookup s [] = s
replaceFromLookup s ((key, value):ls) = replace key value $ replaceFromLookup s ls

replaceFromInverseLookup :: Text -> SymbolDict -> Text
replaceFromInverseLookup s [] = s
replaceFromInverseLookup s ((key, value):ls) = replace value key $ replaceFromInverseLookup s ls

replaceSpecialSymbols :: Text -> Text
replaceSpecialSymbols s = replaceFromLookup s symbolLookup

replaceSpecialSymbolsInverse :: Text -> Text
replaceSpecialSymbolsInverse s = replaceFromInverseLookup s symbolLookup

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

trimText :: Text -> Text
trimText = pack . trim . unpack

evalPath :: FESequent -> FormulaPath -> FEStep
evalPath sequent path = ep path (SubProof $ _steps sequent)
  where
    ep (idx:tail) currentProof = case currentProof of
      SubProof p -> ep tail $ p !! idx
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

insertBeforeProof :: FormulaPath -> FEStep -> FESequent -> FESequent
insertBeforeProof path insertThis = replaceSteps f
  where
    f steps = concat (zipWith (\p idx -> rl [idx] p) steps [0..])

    rl currentPath (SubProof p)
      | path == currentPath = [res, insertThis]
      | otherwise = [res]
        where res = SubProof $ concat $ zipWith (\p idx -> rl (currentPath ++ [idx]) p) p [0..]
    rl currentPath f@(Line _ _)
      | path == currentPath = [f, insertThis]
      | otherwise = [f]

insertAfterProof :: FormulaPath -> FEStep -> FESequent -> FESequent
insertAfterProof path insertThis = replaceSteps f
  where
    f steps = concat (zipWith (\p idx -> rl [idx] p) steps [0..])

    rl currentPath (SubProof p)
      | path == currentPath = [res, insertThis]
      | otherwise = [res]
        where res = SubProof $ concat $ zipWith (\p idx -> rl (currentPath ++ [idx]) p) p [0..]
    rl currentPath f@(Line _ _)
      | path == currentPath = [f, insertThis]
      | otherwise = [f]

removeFromProof :: FormulaPath -> FESequent -> FESequent
removeFromProof path = replaceSteps f
  where
    f steps = if null res then startProof else res
      where
        startProof = [Line "" ""]
        res = filterValid $ zipWith (\p idx -> rl path [idx] p) steps [0..]

    rl removePath currentPath (SubProof p)
      | removePath == currentPath = Nothing
      | otherwise = Just $ SubProof $ filterValid $ zipWith (\p idx -> rl removePath (currentPath ++ [idx]) p) p [0..]
    rl removePath currentPath f@(Line _ _)
      | removePath == currentPath = Nothing
      | otherwise = Just f

    filterValid = filter validateProof . catMaybes
    validateProof (SubProof []) = False
    validateProof _ = True

removePremiseFromProof :: Int -> FESequent -> FESequent
removePremiseFromProof idx sequent = FESequent premises conclusion steps
  where
    premises = _premises sequent ^.. folded . ifiltered (\i _ -> i /= idx)
    conclusion = _conclusion sequent
    steps = _steps sequent

addPremiseToProof :: FESequent -> FESequent
addPremiseToProof sequent = FESequent premises conclusion steps
  where
    premises = _premises sequent ++ [""]
    conclusion = _conclusion sequent
    steps = _steps sequent

editPremisesInProof :: Int -> Text -> FESequent -> FESequent
editPremisesInProof idx newText sequent = FESequent premises conclusion steps
  where
    premises = _premises sequent & element idx .~ newText
    conclusion = _conclusion sequent
    steps = _steps sequent

editConclusionInProof :: Text -> FESequent -> FESequent
editConclusionInProof newText sequent = FESequent premises conclusion steps
  where
    premises = _premises sequent
    conclusion = newText
    steps = _steps sequent

editFormulaInProof :: FormulaPath -> Int -> Text -> FESequent -> FESequent
editFormulaInProof path arg newText = replaceSteps f
  where
    f steps = zipWith (\p idx -> el path arg newText [idx] p) steps [0..]
    el editPath arg newText currentPath (SubProof p) = SubProof $ zipWith (\p idx -> el editPath arg newText (currentPath ++ [idx]) p) p [0..]
    el editPath arg newText currentPath f@(Line statement rule)
      | editPath == currentPath = case arg of
        0 -> Line newText rule
        1 -> Line statement newText
        _ -> error "Invalid field, should be 0 or 1"
      | otherwise = f

replaceInProof :: FormulaPath -> (FEStep -> FEStep) -> FESequent -> FESequent
replaceInProof path replaceWith = replaceSteps f
  where
    f steps = zipWith (\p idx -> rl [idx] p) steps [0..]
    rl currentPath f@(SubProof p)
      | path == currentPath = replaceWith f
      | otherwise = SubProof $ zipWith (\p idx -> rl (currentPath ++ [idx]) p) p [0..]
    rl currentPath f@(Line _ _)
      | path == currentPath = replaceWith f
      | otherwise = f

replaceSteps :: ([FEStep] -> [FEStep]) -> FESequent -> FESequent
replaceSteps f sequent = FESequent premises conclusion steps
 where
    premises = _premises sequent
    conclusion = _conclusion sequent
    steps = f $ _steps sequent

applyOnCurrentProof :: AppModel -> (FESequent -> FESequent) -> [EventResponse AppModel e sp ep]
applyOnCurrentProof model f = actions
  where
    actions = fromMaybe [] (fileIndex >>= Just . getActions)
    fileIndex = cf >>= getProofFileIndexByPath (model ^. tmpLoadedFiles)
    cf = model ^. currentFile

    getActions fileIndex = [
        Model $ model
          & tmpLoadedFiles . singular (ix fileIndex) . parsedSequent %~ f
          & tmpLoadedFiles . singular (ix fileIndex) . isEdited .~ True
      ]

getCurrentSequent :: AppModel -> Maybe FESequent
getCurrentSequent model = sequent
  where
    sequent = fileIndex >>= Just . getSequent
    fileIndex = cf >>= getProofFileIndexByPath (model ^. tmpLoadedFiles)
    cf = model ^. currentFile

    getSequent fileIndex = model ^. tmpLoadedFiles . singular (ix fileIndex) . parsedSequent

firstKeystroke :: [(Text, AppEvent, Bool)] -> WidgetNode s AppEvent -> WidgetNode s AppEvent
firstKeystroke ((key, event, enabled):xs) widget = keystroke_ [(key, if enabled then event else NoEvent)] [ignoreChildrenEvts] (firstKeystroke xs widget)
firstKeystroke [] widget = widget

fontListToText :: [String] -> Text
fontListToText fontList | head fontList == "Regular" = "Default"
                        | head fontList == "Roboto_Regular" = "Roboto"
                        | head fontList == "Comic_Sans_Regular" = "Comic Sans"
                        | head fontList == "Dyslexic" = "Dyslexic"
                        | otherwise = "forgor_to_label"