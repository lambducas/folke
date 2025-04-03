{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.BuildUI (
  buildUI
) where

import Prelude hiding (span)

import Frontend.Types
import Frontend.SpecialCharacters
import Frontend.Helper
import Frontend.Themes
import Frontend.Components.Labels
import Frontend.Components.RenderMarkdown (renderMarkdown)

import Monomer
import Monomer.Widgets.Singles.Base.InputField (InputFieldState (_ifsCurrText, _ifsCursorPos))
import qualified Monomer.Lens as L
import Control.Lens
import TextShow ( TextShow(showt) )
import Data.Text (Text, pack, intercalate, splitOn)
import qualified Data.Text (length)
import Data.List (sort, groupBy)
import Data.Default ( Default(def) )
import Data.String (fromString)
import System.FilePath (takeExtension, takeFileName, takeBaseName)
import System.FilePath.Posix ((</>))
import Data.Maybe (fromMaybe)
import qualified Data.Map

import Backend.Environment (newEnv)
import Backend.Types (Env(Env))
import Frontend.Preferences (preferencePath)

-- import Monomer.Widgets.Containers.TextFieldSuggestions

menuBarCategories :: [(Text, [(Text, Text, AppEvent)])]
menuBarCategories = [
    ("File", [
      ("New Proof", "Ctrl+N", OpenCreateProofPopup),
      ("Save File", "Ctrl+S", SaveCurrentFile),
      ("Close File", "Ctrl+W", CloseCurrentFile)
    ]),
    ("Edit", [
      ("Make Subproof", "Ctrl+Tab", NoEvent),
      ("Undo Subproof", "Ctrl+Shift+Tab", NoEvent),
      ("Goto Next Input", "Return", NoEvent),
      ("Insert Line Below", "Ctrl+Enter", NoEvent),
      ("Close Subproof", "Ctrl+Enter", NoEvent)
    ]),
    ("Preferences", [
      ("Open Preferences", "Ctrl+Shift+P", OpenFile_ preferencePath "")
    ]),
    ("Help", [
      ("Open Guide", "", OpenFile_ "user_guide_en.md" "./docs")
    ])
  ]

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI _wenv model = widgetTree where
  selTheme = getActualTheme $ model ^. preferences . selectedTheme
  popupBackground = selTheme ^. L.userColorMap . at "popupBackground" . non def
  backgroundColor = selTheme ^. L.userColorMap . at "backgroundColor" . non def
  selectedColor = selTheme ^. L.userColorMap . at "selectedFileBg" . non def
  dividerColor = selTheme ^. L.userColorMap . at "dividerColor" . non def
  hoverColor = selTheme ^. L.userColorMap . at "hoverColor" . non def
  proofBoxColor = selTheme ^. L.userColorMap . at "proofBoxColor" . non def

  h1 = Frontend.Components.Labels.h1 model
  h2 = Frontend.Components.Labels.h2 model
  h3 = Frontend.Components.Labels.h3 model
  span = Frontend.Components.Labels.span model
  span_ = Frontend.Components.Labels.span_ model
  symbolSpan = Frontend.Components.Labels.symbolSpan model
  symbolSpan_ = Frontend.Components.Labels.symbolSpan_ model
  paragraph = Frontend.Components.Labels.paragraph model
  -- paragraph_ = Frontend.Components.Labels.paragraph_ model
  iconLabel = Frontend.Components.Labels.iconLabel model
  iconButton = Frontend.Components.Labels.iconButton model
  trashButton = Frontend.Components.Labels.trashButton model
  bold = Frontend.Components.Labels.bold model
  normalStyle = Frontend.Components.Labels.normalStyle model
  symbolStyle = Frontend.Components.Labels.symbolStyle model

  button a b = Monomer.button a b `styleBasic` [textSize u]
  fastTooltip tip widget = Monomer.tooltip_ tip [tooltipDelay 400] widget `styleBasic` [textSize u]

  globalKeybinds = [
      ("Ctrl-n", OpenCreateProofPopup, True),
      ("Ctrl-s", SaveCurrentFile, True),
      ("Ctrl-w", CloseCurrentFile, True),
      ("Ctrl-Shift-p", OpenFile_ preferencePath "", True)
    ]

  widgetTree = firstKeystroke globalKeybinds $ themeSwitch_ selTheme [themeClearBg] $ vstack [
      vstack [
        menuBar,
        mainContent
      ],
      popup_ confirmDeletePopup [popupAlignToWindow, popupDisableClose, alignCenter, alignMiddle] (vstack_ [childSpacing] [
        h1 "Close without saving?",
        span "Are you sure you want to close",
        bold $ span (showt $ model ^. confirmDeleteTarget),
        span "without saving. All changes will be lost!",
        spacer,
        hstack_ [childSpacing] [
          normalStyle $ toggleButton "Cancel" confirmDeletePopup,
          normalStyle $ button "Close anyway" (maybe NoEvent CloseFileSuccess (model ^. confirmDeleteTarget))
        ]
      ] `styleBasic` [bgColor popupBackground, border 1 dividerColor, padding 20, textSize $ u -2])
    ]

  menuBar = hstack (zipWith menuBarButton menuBarCategories [0..])
    `styleBasic` [borderB 1 dividerColor, padding 5, textSize $ u -2]
    where
      menuBarButton (name, actions) idx = vstack [
          box_ [onClick (SetOpenMenuBarItem (Just idx))] (
            span name
              `styleBasic` [textSize $ u -2, radius 4, paddingV 5, paddingH 10]
              `styleHover` [bgColor selectedColor]
          ),
          popupV (Just idx == model ^. openMenuBarItem) (\s -> if s then SetOpenMenuBarItem (Just idx) else SetOpenMenuBarItem Nothing)
            (vstack (map dropdownButton actions)
              `styleBasic` [width 300, bgColor popupBackground, border 1 dividerColor, padding 4, radius 4, textSize $ u -2])
        ]

      dropdownButton (name, keybind, action) = box_ [onClick action, onClick (SetOpenMenuBarItem Nothing), expandContent] $ hstack [
          span name `styleBasic` [textSize $ u -2],
          filler,
          span keybind `styleBasic` [textSize $ u -2]
        ]
          `styleBasic` [radius 4, paddingV 10, paddingH 20, cursorHand, textSize $ u -2]
          `styleHover` [bgColor hoverColor]

  mainContent = hstack [
      fileWindow,
      editWindow,
      ruleWindow
    ] `styleBasic` [expandHeight 1000]

  fileWindow = case model ^. preferences . workingDir of
    Nothing -> vstack [
        box_ [expandContent] (hstack [
            bold (span "File Explorer"),
            filler,
            fastTooltip "Set working directory" $ iconButton remixFolderUserFill OpenSetWorkingDir
              `styleBasic` [bgColor transparent, border 1 transparent, padding 4, textSize u]
              `styleHover` [bgColor hoverColor]
          ]) `styleBasic` [borderB 1 dividerColor, paddingV 2, paddingH 16],

        vstack_ [childSpacing] [
          paragraph "No folder has been opened. Open a folder where you have your proofs stored.",
          box (button "Open Folder" OpenSetWorkingDir)
        ] `styleBasic` [padding u]
      ] `styleBasic` [ width 250, borderR 1 dividerColor ]

    Just _ -> vstack [
        box_ [expandContent] (hstack [
            bold (span "File Explorer"),
            filler,
            fastTooltip "Create new proof" $ iconButton remixFileAddLine OpenCreateProofPopup
              `styleBasic` [bgColor transparent, border 1 transparent, padding 4, textSize u]
              `styleHover` [bgColor hoverColor],
            fastTooltip "Refresh files" $ iconButton remixRestartLine RefreshExplorer
              `styleBasic` [bgColor transparent, border 1 transparent, padding 4, textSize u]
              `styleHover` [bgColor hoverColor],
            fastTooltip "Set working directory" $ iconButton remixFolderUserFill OpenSetWorkingDir
              `styleBasic` [bgColor transparent, border 1 transparent, padding 4, textSize u]
              `styleHover` [bgColor hoverColor],

            let cep = (CreateEmptyProof $ model ^. newFileName) in
              popup_ newFilePopupOpen [popupAlignToWindow, alignCenter, alignMiddle] (vstack [
                h2 "Create proof",
                spacer,
                span "Enter the name of your proof",
                spacer,
                firstKeystroke [("Enter", cep, True)] $ textField_ newFileName [placeholder "my_proof"],
                spacer,
                button "+ Create proof" cep
              ] `styleBasic` [bgColor popupBackground, border 1 dividerColor, padding (1.5*u), width (20*u)])
          ]) `styleBasic` [borderB 1 dividerColor, paddingV 2, paddingH 16],

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
                  span ((head . fst . head) seqs),
                  iconLabel remixFolder5Line `styleBasic` [paddingL 8]
                ] `styleBasic` [paddingL (16 * indent), paddingV 8],
                fileTreeUI newParts (indent + 1)
              ]
              where
                newParts = map (\f -> ((tail . fst) f, snd f)) seqs
                -- newParts = map (\f -> (splitOn "/" (pack f), f)) newFiles
                -- newFiles = map (unpack . intercalate "/" . tail . fst) seqs

  fileItem indent text filePath = box_ [expandContent, onClick (OpenFile filePath)] $ hstack_ [childSpacing] [
      iconLabel iconIdent `styleBasic` [fromMaybe mempty (iconColor >>= Just . textColor)],
      span_ text [ellipsis]
    ]
      `styleHover` [styleIf (not isCurrent) (bgColor hoverColor)]
      `styleBasic` [borderB 1 dividerColor, paddingL (16 * indent), paddingR 16, paddingV 8, cursorHand, styleIf isCurrent (bgColor selectedColor)]
    where
      isCurrent = case model ^. preferences . workingDir of
        Nothing -> False
        Just wd -> (model ^. currentFile) == Just (wd </> filePath)
      ext = takeExtension filePath
      iconIdent
        | ext == ".md" = remixMarkdownFill
        | ext == "." <> feFileExt = remixBracesFill --remixSurveyFill
        | otherwise = remixMenu2Line
      iconColor
        | ext == ".md" = Just $ rgb 94 156 255
        | ext == "." <> feFileExt = Just $ rgb 255 130 0 --Just $ rgb 255 130 0
        | otherwise = Nothing

  editWindow = vstack [
      fileNavBar (model ^. openFiles),
      proofWindow (model ^. currentFile)
    ]

  fileNavBar filePaths = hscroll (hstack (map boxedLabel filePaths))
    `styleBasic` [bgColor selectedColor, maxHeight 50, minHeight 50, height 50]
    where
      boxedLabel filePath = box_ [expandContent, onClick (SetCurrentFile filePath)] $ hstack [
          spacer,
          fastTooltip (pack filePath) $ span displayName,
          fastTooltip "Close tab" $
            box_ [onClick (CloseFile filePath)] (symbolSpan closeText
              `styleBasic` [textSize (1.5*u), radius 8, padding 4]
              `styleHover` [bgColor hoverColor])
        ]
          `styleBasic` [borderR 1 dividerColor, styleIf isCurrent (bgColor backgroundColor), cursorHand]
          `styleHover` [styleIf (not isCurrent) (bgColor hoverColor)]
          where
            displayName = pack $ takeFileName filePath --pack filePath
            closeText = if isFileEdited file then "●" else "⨯"
            file = getProofFileByPath (model ^. tmpLoadedFiles) filePath
            isCurrent = (model ^. currentFile) == Just filePath

  proofWindow Nothing = vstack [] `styleBasic` [expandWidth 1000] -- Don't know how expandWith works, but it works
  proofWindow (Just fileName) = case file of
    Nothing -> span "Filepath not loaded"
    Just (PreferenceFile _ _) -> hstack [
      vstack [
          h3 "App scale",
          hstack_ [childSpacing] [
            symbolSpan (showDecimals 2 (model ^. preferences . appScale)),
            hslider_ (preferences . appScale) 0.25 2 [thumbVisible]
          ],
          spacer, spacer,

          h3 "Choose font:",
          textDropdown_ (preferences . selectNormalFont) [
            ["Regular","Medium","Bold"],
            ["Dyslexic"],
            ["Roboto_Regular","Roboto_Medium","Roboto_Bold"],
            ["Comic_Sans_Thin", "Comic_Sans_Regular", "Comic_Sans_Medium", "Comic_Sans_Bold"]
            ] fontListToText [onChange UpdateFont] `styleBasic` [textSize u],
          spacer,

          h3 "Set font thickness:",
          textDropdown_ (preferences . normalFont) (model ^. preferences . selectNormalFont) pack [] `styleBasic` [textSize u],
          vstack $ map illustThickness (model ^. preferences . selectNormalFont),
          spacer, spacer,

          h3 "Set symbolic font thickness:",
          paragraph "The symbolic font is the font used in logic proofs",
          textDropdown_ (preferences . logicFont) ["Symbol_Regular","Symbol_Medium","Symbol_Bold"] pack [] `styleBasic` [textSize u],
          spacer, spacer,

          h3 "Theme",
          normalStyle $ button "Switch light/dark mode" SwitchTheme
        ] `styleBasic` [maxWidth 1024]
      ] `styleBasic` [padding 30]
      where illustThickness fontThicknessess = vstack [label "This is how thick I am" `styleBasic` [textFont $ fromString fontThicknessess, textSize u]]
    Just file@(ProofFile {}) -> case parsedSequent of
      Nothing -> vstack [
          vstack [
            h1 $ pack $ _path file,
            spacer
          ] `styleBasic` [padding 10, borderB 1 dividerColor],
          vscroll_ [wheelRate 50] (vstack [
            paragraph "Corrupt proof! Try editing the proof-file in a text editor to fix it. Close this tab and reopen the proof-file after corrupted data is removed",
            spacer,
            paragraph "File preview:",
            spacer,
            symbolSpan_ (_content file) [multiline]
          ]) `styleBasic` [padding 10]
        ]
      Just parsedSequent -> keystroke [("Ctrl-s", SaveFile file)] $ vstack [
          vstack [
            h1 heading,
            spacer,
            subheading
          ] `styleBasic` [padding 10, borderB 1 dividerColor],
          scroll_ [wheelRate 50] (proofTreeUI parsedSequent) `styleBasic` [padding 10],
          hstack [
            proofStatusLabel,
            filler,
            button "Save proof" (SaveFile file) `styleBasic` [textSize u],
            spacer,
            button "Check proof" (CheckProof file) `styleBasic` [textSize u]
          ] `styleBasic` [padding 10, borderT 1 dividerColor]
        ]
        where
          heading = (pack . takeBaseName . _path) file
          subheading
            | null premises && conclusion == "" = span "Empty proof"
            | otherwise = symbolSpan prettySequent
          prettySequent = intercalate ", " premises <> " ⊢ " <> conclusion
          conclusion = replaceSpecialSymbols (_conclusion parsedSequent)
          premises = map replaceSpecialSymbols (_premises parsedSequent)
      where
        parsedSequent = _parsedSequent file
    Just (MarkdownFile _p content) -> vscroll_ [wheelRate 50] (renderMarkdown model content `styleBasic` [padding u, maxWidth 300]) `nodeKey` "markdownScroll"
    Just (OtherFile p content) -> vscroll_ [wheelRate 50] $ vstack_ [childSpacing] [
        label $ pack p <> ": This file type is not supported",
        paragraph content
      ] `styleBasic` [padding u]
    where file = getProofFileByPath (model ^. tmpLoadedFiles) fileName

  proofStatusLabel = case model ^. proofStatus of
    Nothing -> span "Checking proof..." `styleBasic` [textColor orange]
    Just (Left error) -> span ("Proof is incorrect: " <> pack error) `styleBasic` [textColor red]
    Just (Right _) -> span "Proof is correct :)" `styleBasic` [textColor lime]

  proofTreeUI :: FESequent -> WidgetNode AppModel AppEvent
  proofTreeUI sequent = vstack [
      vstack_ [childSpacing] [
        h2 "Premises",
        vstack_ [childSpacing] $ zipWith premiseLine (_premises sequent) [0..],
        widgetIf (null $ _premises sequent) (span "No premises")
      ],
      spacer,
      hstack [button "+ Premise" AddPremise `styleBasic` [textSize u]],
      spacer, spacer,

      vstack_ [childSpacing] [
        h2 "Conclusion",
        spacer,
        box_ [alignLeft] (
          symbolStyle $ textFieldV_ (replaceSpecialSymbols (_conclusion sequent)) EditConclusion [placeholder "Enter conclusion here"]
            `styleBasic` [maxWidth 400]
        )
          --`styleBasic` [textFont $ fromString $ model ^. logicFont, textSize u],
      ],
      spacer, spacer,

      h2 "Proof",
      spacer,
      hstack [
        lineNumbers,
        tree
      ],

      -- Hack so last proof line can scroll all the way to the top
      box (label "") `styleBasic` [height 1000]
    ]
    where
      premiseLine premise idx = box_ [alignLeft] (hstack [
          symbolStyle $ textFieldV_ (replaceSpecialSymbols premise) (EditPremise idx) [placeholder "Enter premise"]
            `nodeKey` ("premise.input." <> showt idx),
            --`styleBasic` [textFont $ fromString $ model ^. logicFont, textSize u],
          spacer,
          fastTooltip "Remove line" $ trashButton (RemovePremise idx),
          spacer
        ] `styleBasic` [maxWidth 400]) `nodeKey` ("premise.line." <> showt idx)

      tree = vstack [
          ui,
          spacer,
          hstack_ [childSpacing] [
            button "+ New line" AddLine `styleBasic` [textSize u],
            button "+☐ New sub proof" AddSubProof `styleBasic` [textSize u]
          ]
        ]
        where
          ui = vstack_ [childSpacing] (ghostPremises ++ map fst s)
          s = getSubProof (_steps sequent) [] 0 1

          ghostPremises = map ghostPremise (_premises sequent)
          ghostPremise premise = hstack [
              symbolSpan pp,
              filler,
              symbolSpan "premise" `styleBasic` [width 300, paddingH 10, textSize u],
              spacer,
              vstack [] `styleBasic` [width 300]
            ] `styleBasic` [height 34]
            where pp = replaceSpecialSymbols premise

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
                -- symbolSpan (showt index <> ".") `styleBasic` [textFont $ fromString $ model ^. logicFont],
                -- spacer,

                -- textFieldV "" (\_ -> NoEvent),

                firstKeystroke [
                  ("Up", FocusOnKey $ WidgetKey (showt (index - 1) <> ".statement"), prevIndexExists),
                  ("Down", FocusOnKey $ WidgetKey (showt (index + 1) <> ".statement"), nextIndexExists),
                  -- ("Right", FocusOnKey $ WidgetKey (showt index <> ".rule"), True),

                  ("Ctrl-Tab", SwitchLineToSubProof path (WidgetKey $ showt index <> ".statement"), True),
                  ("Ctrl-Shift-Tab", SwitchSubProofToLine pathToParentSubProof (WidgetKey $ showt index <> ".statement"), True),
                  ("Delete", RemoveLine path, trashActive),
                  ("Backspace", RemoveLine path, canBackspaceToDelete),
                  ("Ctrl-Enter", InsertLineAfter path, not isLastLine),
                  ("Ctrl-Enter", InsertLineAfter pathToParentSubProof, isLastLine),
                  ("Enter", NextFocus 1, True)
                ] (symbolStyle $ textFieldV_ (replaceSpecialSymbols statement) (EditLine path 0) [onKeyDown handleFormulaKey, placeholder "Empty statement"]
                  `nodeKey` (showt index <> ".statement"))
                    `nodeKey` (showt index <> ".statement.keystroke"), 

                spacer,

                firstKeystroke [
                  ("Up", FocusOnKey $ WidgetKey (showt (index - 1) <> ".rule"), prevIndexExists),
                  ("Down", FocusOnKey $ WidgetKey (showt (index + 1) <> ".rule"), nextIndexExists),
                  -- ("Left", FocusOnKey $ WidgetKey (showt index <> ".statement"), True),

                  ("Ctrl-Tab", SwitchLineToSubProof path (WidgetKey $ showt index <> ".rule"), True),
                  ("Ctrl-Shift-Tab", SwitchSubProofToLine pathToParentSubProof (WidgetKey $ showt index <> ".rule"), True),
                  ("Delete", RemoveLine path, trashActive),
                  ("Backspace", FocusOnKey (WidgetKey (showt index <> ".statement")), rule == ""),
                  ("Ctrl-Enter", InsertLineAfter pathToParentSubProof, isLastLine),
                  ("Enter", InsertLineAfter path, True)
                ] (symbolStyle $ textFieldV_ (replaceSpecialSymbols rule) (EditLine path 1) [onKeyDown handleRuleKey, placeholder "No rule"]
                  `nodeKey` (showt index <> ".rule"))
                    `nodeKey` (showt index <> ".rule.keystroke")
                    `styleBasic` [width 300]

                -- , textFieldSuggestionsV rule (\_i t -> EditLine path 1 t) allRules (const $ textFieldV (replaceSpecialSymbols rule) (EditLine path 1)) label `styleHover` [bgColor transparent]
              ],
              spacer,
              b
            ] `nodeKey` showt index

          -- allRules = [replaceSpecialSymbols rule] ++ (filter (\f -> (replaceSpecialSymbols . toLower) rule `isInfixOf` toLower f) $ map (pack . fst) (Data.Map.toList $ rules newEnv))--[model ^. userLens, "Thecoder", "another", "bruh", "tesdt", "dsjhnsifhbsgfsghffgusgfufgssf", "1", "2"]
          --   where rules (Env _ _ r _ _ _ _ _) = r

          handleFormulaKey, handleRuleKey :: (KeyMod, KeyCode, InputFieldState Text) -> AppEvent
          handleFormulaKey (_mod, code, state)
            | isKeyRight code && isAtEnd = FocusOnKey $ WidgetKey (showt index <> ".rule")
            | otherwise = NoEvent --Print $ show state
            where
              isAtEnd = cursorPos == textLen
              cursorPos = _ifsCursorPos state
              textLen = Data.Text.length (_ifsCurrText state)

          handleRuleKey (_mod, code, state)
            | isKeyLeft code && isAtBeginning = FocusOnKey $ WidgetKey (showt index <> ".statement")
            | otherwise = NoEvent --Print $ show state
            where
              isAtBeginning = cursorPos == 0
              cursorPos = _ifsCursorPos state

          b = box $ hstack_ [childSpacing] [
                fastTooltip "Remove line" $
                  trashButton (RemoveLine path)
                    `nodeEnabled` trashActive,

                fastTooltip "Insert line below" $
                  button "↓+" (InsertLineAfter path) `styleBasic` [textSize u],

                -- fastTooltip "Insert subproof below" $
                --   button "↓☐+" (InsertSubProofAfter path),

                fastTooltip "Convert line to subproof" $
                  button "→☐" (SwitchLineToSubProof path (WidgetKey $ showt index <> ".statement")) `styleBasic` [textSize u],
                
                widgetIf isSubProofSingleton $
                  fastTooltip "Undo subproof" $
                    button "☒" (SwitchSubProofToLine pathToParentSubProof (WidgetKey $ showt index <> ".statement")) `styleBasic` [textSize u],
                
                widgetIf (isLastLine && nextIndexExists) $
                  fastTooltip "Close subproof" $
                    button "⏎" (InsertLineAfter pathToParentSubProof) `styleBasic` [textSize u]
                
                -- widgetIf isLastLine (button "/[]+" (InsertSubProofAfter pathToParentSubProof))
              ] `styleBasic` [width 300]

          canBackspaceToDelete = rule == "" && statement == "" && trashActive
          trashActive = not (index == 1 && not nextIndexExists && statement == "" && rule == "")
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




      lineNumbers = ui
        where
          ui = vstack_ [childSpacing] (ghostLines ++ map fst s)
          s = getSubProof2 (_steps sequent) [] 0 (length (_premises sequent) + 1)

          ghostLines = map ghostLine [1..length (_premises sequent)]
          ghostLine index = symbolSpan (showt index <> ".")
            `styleBasic` [width 48, paddingR 12, height 34, textRight]

      ln (SubProof p) index path = (ui, lastIndex)
        where
          ui = vstack_ [childSpacing] (map fst s) `styleBasic` [border 1 transparent, paddingV 8]
          lastIndex = if null s then index else snd $ last s
          s = getSubProof2 p path 0 index

      ln (Line _ _) index _path = (ui, lastIndex)
        where
          ui = symbolSpan (showt index <> ".")
            `styleBasic` [width 48, paddingR 12, height 34, textRight]
            `nodeKey` showt index <> "label"
          lastIndex = index + 1

      getSubProof2 p path arrayIndex visualIndex
        | arrayIndex < length p = u : getSubProof2 p path (arrayIndex + 1) (snd u)
        | otherwise = []
          where u = ln (p !! arrayIndex) visualIndex (path ++ [arrayIndex])

  ruleWindow = vscroll (vstack_ [childSpacing] [
      h2 "Rules",
      vstack $ map (label . pack .fst) (Data.Map.toList $ rules newEnv)
    ] `styleBasic` [padding u]) `styleBasic` [width 300]
    where rules (Env _ _ r _ _ _ _ _) = r