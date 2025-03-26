{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.BuildUI (
  buildUI
) where

import Prelude hiding (span)

import Frontend.Types
import Frontend.SpecialCharacters
import Frontend.Helper
import Frontend.Components.Labels
import Frontend.Components.RenderMarkdown (renderMarkdown)

import Monomer
import qualified Monomer.Lens as L
import Control.Lens
import TextShow ( TextShow(showt) )
import Data.Text (Text, pack, intercalate, splitOn)
import Data.List (sort, groupBy)
import Data.Default ( Default(def) )
import Data.String (fromString)
import System.FilePath (takeExtension)
import Data.Maybe (fromMaybe)

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

  h1 = Frontend.Components.Labels.h1 model
  h2 = Frontend.Components.Labels.h2 model
  span = Frontend.Components.Labels.span model
  span_ = Frontend.Components.Labels.span_ model
  symbolSpan = Frontend.Components.Labels.symbolSpan model
  symbolSpan_ = Frontend.Components.Labels.symbolSpan_ model
  paragraph = Frontend.Components.Labels.paragraph model
  paragraph_ = Frontend.Components.Labels.paragraph_ model
  iconLabel = Frontend.Components.Labels.iconLabel model
  iconButton = Frontend.Components.Labels.iconButton model
  trashButton = Frontend.Components.Labels.trashButton model
  bold = Frontend.Components.Labels.bold model

  widgetTree = themeSwitch_ selTheme [themeClearBg] $ vstack [
      vstack [
        menuBar,
        mainContent
      ],
      popup_ confirmDeletePopup [popupAlignToWindow, popupDisableClose, alignCenter, alignMiddle] (vstack_ [childSpacing] [
        h1 "Close without saving?",
        paragraph "Are you sure you want to close",
        paragraph (showt $ model ^. confirmDeleteTarget),
        paragraph "without saving. All changes will be lost!",
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
            span name
              `styleBasic` [textSize 14, radius 4, paddingV 5, paddingH 10]
              `styleHover` [bgColor selectedColor]
          ),
          popupV (Just idx == model ^. openMenuBarItem) (\s -> if s then SetOpenMenuBarItem (Just idx) else SetOpenMenuBarItem Nothing)
            (vstack (map dropdownButton actions)
              `styleBasic` [width 300, bgColor popupBackground, border 1 dividerColor, padding 4, radius 4])
        ]

      dropdownButton (name, keybind, action) = box_ [onClick action, onClick (SetOpenMenuBarItem Nothing), expandContent] $ hstack [
          span name `styleBasic` [textSize 14],
          filler,
          span keybind `styleBasic` [textSize 14]
        ]
          `styleBasic` [radius 4, paddingV 10, paddingH 20, cursorHand]
          `styleHover` [bgColor hoverColor]

  mainContent = hstack [
      fileWindow,
      editWindow
    ] `styleBasic` [expandHeight 1000]

  fileWindow = vstack [
      box_ [expandContent] (hstack [
          bold (span "File Explorer"),
          filler,
          iconButton remixFileAddLine OpenCreateProofPopup
            `styleBasic` [bgColor transparent, border 1 transparent, padding 4]
            `styleHover` [bgColor hoverColor],

          let cep = (CreateEmptyProof $ model ^. newFileName) in
            popup_ newFilePopupOpen [popupAlignToWindow, alignCenter, alignMiddle] (vstack [
              h2 "Create proof",
              paragraph "Enter the name of your proof",
              spacer,
              firstKeystroke [("Enter", cep, True)] $ textField_ newFileName [placeholder "my_proof"],
              spacer,
              button "+ Create proof" cep
            ] `styleBasic` [bgColor popupBackground, padding 10])
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
      isCurrent = (model ^. currentFile) == Just filePath
      ext = takeExtension filePath
      iconIdent = case ext of
        ".md" -> remixMarkdownFill
        ".logic" -> remixSurveyFill
        _ -> remixMenu2Line
      iconColor = case ext of
        ".md" -> Just $ rgb 94 156 255
        ".logic" -> Just $ rgb 255 130 0
        _ -> Nothing

  editWindow = vstack [
      fileNavBar (model ^. openFiles),
      proofWindow (model ^. currentFile)
    ]

  fileNavBar filePaths = hscroll (hstack (map boxedLabel filePaths))
    `styleBasic` [bgColor selectedColor, maxHeight 50, minHeight 50, height 50]
    where
      boxedLabel filePath = box_ [expandContent, onClick (SetCurrentFile filePath)] $ hstack [
          spacer,
          span displayName,
          box_ [onClick (CloseFile filePath)] (symbolSpan closeText
            `styleBasic` [textFont $ fromString $ model ^. logicFont, textSize (1.5*u), radius 8, padding 4]
            `styleHover` [bgColor hoverColor])
        ]
          `styleBasic` [borderR 1 dividerColor, styleIf isCurrent (bgColor backgroundColor), cursorHand]
          `styleHover` [styleIf (not isCurrent) (bgColor hoverColor)]
          where
            displayName = pack filePath
            closeText = if isFileEdited file then "●" else "⨯"
            file = getProofFileByPath (model ^. tmpLoadedFiles) filePath
            isCurrent = (model ^. currentFile) == Just filePath

  proofWindow Nothing = vstack [] `styleBasic` [expandWidth 1000] -- Don't know how expandWith works, but it works
  proofWindow (Just fileName) = case file of
    Nothing -> span "Filepath not loaded"
    Just (SettingsFile _) -> hstack [
      vstack [
          label "Choose font" `styleBasic` [textFont $ fromString $ model ^. normalFont],
          textDropdown_ selectNormalFont [
            ["Regular","Medium","Bold"],
            ["Dyslexic"],
            ["Roboto_Regular","Roboto_Medium","Roboto_Bold"],
            ["Comic_Sans_Regular", "Comic_Sans_Thin", "Comic_Sans_Medium", "Comic_Sans_Bold"]
            ] fontListToText [onChange UpdateFont],
          label "This is how I look" `styleBasic` [textFont $ fromString $ head $ model ^. selectNormalFont],
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
      where illustThickness fontThicknessess = vstack [label "This is how thick I am" `styleBasic` [textFont $ fromString fontThicknessess]]
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
      Just parsedSequent -> keystroke [("Ctrl-s", SaveProof file), ("Ctrl-w", CloseCurrentFile)] $ vstack [
          vstack [
            h1 $ pack $ _path file,
            spacer,
            symbolSpan prettySequent
          ] `styleBasic` [padding 10, borderB 1 dividerColor],
          scroll_ [wheelRate 50] (proofTreeUI parsedSequent) `styleBasic` [padding 10],
          hstack [
            proofStatusLabel,
            filler,
            button "Save proof" (SaveProof file),
            spacer,
            button "Check proof" (CheckProof file)
          ] `styleBasic` [padding 10, borderT 1 dividerColor]
        ]
        where
          prettySequent = intercalate ", " premises <> " ⊢ " <> conclusion
          conclusion = replaceSpecialSymbols (_conclusion parsedSequent)
          premises = map replaceSpecialSymbols (_premises parsedSequent)
      where
        parsedSequent = _parsedSequent file
    Just (MarkdownFile _p content) -> vscroll (renderMarkdown model content `styleBasic` [padding u, maxWidth 300]) `nodeKey` "markdownScroll"
    Just (OtherFile p content) -> vstack_ [childSpacing] [
        label $ pack p <> ": This file type is not supported",
        paragraph content
      ]
    where file = getProofFileByPath (model ^. tmpLoadedFiles) fileName

  proofStatusLabel = case model ^. proofStatus of
    Nothing -> span "Checking proof..." `styleBasic` [textColor orange]
    Just (Left error) -> span ("Proof is incorrect: " <> pack error) `styleBasic` [textColor red]
    Just (Right _) -> span "Proof is correct :)" `styleBasic` [textColor lime]

  proofTreeUI :: FESequent -> WidgetNode AppModel AppEvent
  proofTreeUI sequent = vstack [
      vstack_ [childSpacing] [
        h2 "Premises" `styleBasic` [textFont $ fromString $ last $ model ^. selectNormalFont],
        vstack_ [childSpacing] $ zipWith premiseLine (_premises sequent) [0..],
        widgetIf (null $ _premises sequent) (span "No premises")
      ],
      spacer,
      hstack [button "+ Premise" AddPremise],
      spacer, spacer,

      h2 "Conclusion" `styleBasic` [textFont $ fromString $ last $ model ^. selectNormalFont],
      spacer,
      textFieldV_ (replaceSpecialSymbols (_conclusion sequent)) EditConclusion [placeholder "Enter conclusion here"]
        `styleBasic` [textFont $ fromString $ model ^. logicFont],
      spacer, spacer,

      h2 "Proof" `styleBasic` [textFont $ fromString $ last $ model ^. selectNormalFont],
      spacer,
      hstack [
        lineNumbers,
        tree
      ],

      -- Hack so last proof line can scroll all the way to the top
      box (label "") `styleBasic` [height 1000]
    ]
    where
      premiseLine premise idx = hstack [
          textFieldV_ (replaceSpecialSymbols premise) (EditPremise idx) [placeholder "Enter premise"]
            `styleBasic` [textFont $ fromString $ model ^. logicFont],
          spacer,
          tooltip "Remove line" $ trashButton (RemovePremise idx),
          spacer
        ]

      tree = vstack [
          ui,
          spacer,
          hstack_ [childSpacing] [
            button "+ New line" AddLine,
            button "+☐ New sub proof" AddSubProof
          ]
        ]
        where
          ui = vstack_ [childSpacing] (ghostPremises ++ map fst s)
          s = getSubProof (_steps sequent) [] 0 1

          ghostPremises = map ghostPremise (_premises sequent)
          ghostPremise premise = hstack [
              symbolSpan pp,
              filler,
              symbolSpan "premise" `styleBasic` [width 175, paddingH 10],
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
                  `styleBasic` [textFont $ fromString $ model ^. logicFont, width 175]
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