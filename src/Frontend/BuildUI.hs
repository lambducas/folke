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

import Shared.Messages (FEResult(..))
import Logic.Par (myLexer, pForm)

import Monomer
import Monomer.Widgets.Singles.Base.InputField (InputFieldState (_ifsCurrText, _ifsCursorPos))
import qualified Monomer.Lens as L
import Control.Lens
import TextShow ( TextShow(showt) )
import Data.Text (Text, unpack, pack, intercalate, splitOn, toLower)
import qualified Data.Text (length)
import Data.List (sort, groupBy, isInfixOf)
import Data.Default ( Default(def) )
import Data.String (fromString)
import Data.Either (isLeft)
import System.FilePath (takeExtension, takeFileName, takeBaseName)
import System.FilePath.Posix ((</>))
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Map

-- import Monomer.Widgets.Containers.TextFieldSuggestions

menuBarCategories :: [(Text, [(Text, Text, AppEvent)])]
menuBarCategories = [
    ("File", [
      ("New Proof", "Ctrl+N", CreateEmptyProof),
      ("Save File", "Ctrl+S", SaveCurrentFile),
      ("Close File", "Ctrl+W", CloseCurrentFile),
      ("Exit", "", ExitApp)
    ]),
    ("Edit", [
      -- ("Make Subproof", "Ctrl+Tab", NoEvent),
      -- ("Undo Subproof", "Ctrl+Shift+Tab", NoEvent),
      -- ("Goto Next Input", "Return", NoEvent),
      -- ("Insert Line Below", "Ctrl+Enter", NoEvent),
      -- ("Close Subproof", "Ctrl+Enter", NoEvent),
      ("Validate Proof", "Ctrl+R", CheckCurrentProof)
    ]),
    ("View", [
      ("Toggle File Explorer", "Ctrl+B", ToggleFileExplorer),
      ("Open Preferences", "Ctrl+Shift+P", OpenPreferences),
      ("Open Guide", "", OpenGuide)
    ]),
    ("Help", [
      ("Open Guide", "", OpenGuide)
    ])
  ]

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI _wenv model = widgetTree where
  selTheme = getActualTheme $ model ^. preferences . selectedTheme
  accentColor = selTheme ^. L.userColorMap . at "accent" . non def
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
  u = model ^. preferences . fontSize

  button a b = Monomer.button a b `styleBasic` [textSize u]
  fastTooltip tip widget = Monomer.tooltip_ tip [tooltipDelay 400] widget `styleBasic` [textSize u]
  fastScroll = scroll_ [wheelRate 50]
  fastVScroll = vscroll_ [wheelRate 50]
  fastHScroll = hscroll_ [wheelRate 50]

  globalKeybinds = filter (\(b, _, _) -> b /= "") $ map (\(_, b, e) -> (convertBind b, e, True)) $ concatMap snd menuBarCategories
    where
      convertBind b = intercalate "-" (map fixKey (splitOn "+" b))
      fixKey k
        | Data.Text.length k == 1 = toLower k
        | otherwise = k

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
      actionSidebar,
      fileExplorerSidebar,
      editWindow,
      ruleSidebar
    ] `styleBasic` [expandHeight 100000]

  actionSidebar :: WidgetNode AppModel AppEvent
  actionSidebar = vstack (map actionButton [
      ("Toggle File Explorer (Ctrl+B)", remixFileSearchLine, ToggleFileExplorer, model ^. preferences . fileExplorerOpen),
      ("Toggle Rule Explorer", remixRuler2Line, ToggleRulesSidebar, model ^. preferences . rulesSidebarOpen),
      ("Open Preferences", remixSettings4Line, OpenPreferences, False),
      ("Open Guide", remixQuestionLine, OpenGuide, False)
    ]) `styleBasic` [width 50, borderR 1 dividerColor]
    where
      actionButton (tt, icon, event, selected) = box_ [alignCenter, alignMiddle] btn `styleBasic` [height 50]
        where
          btn = fastTooltip tt $ iconButton icon event
            `styleHover` [bgColor hoverColor]
            `styleBasic` [radius (0.5*u), styleIf selected (textColor accentColor), styleIf selected (bgColor selectedColor)]

  fileExplorerSidebar :: WidgetNode AppModel AppEvent
  fileExplorerSidebar = widgetIf (model ^. preferences . fileExplorerOpen) $ case model ^. preferences . workingDir of
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
      ] `styleBasic` [ width (model ^. preferences . fileExplorerWidth), borderR 1 dividerColor ]

    Just _ -> vstack [
        box_ [expandContent] (hstack [
            bold (span "File Explorer"),
            filler,
            fastTooltip "Create new proof" $ iconButton remixFileAddLine CreateEmptyProof
              `styleBasic` [bgColor transparent, border 1 transparent, padding 4, textSize u]
              `styleHover` [bgColor hoverColor],
            fastTooltip "Refresh files" $ iconButton remixRestartLine RefreshExplorer
              `styleBasic` [bgColor transparent, border 1 transparent, padding 4, textSize u]
              `styleHover` [bgColor hoverColor],
            fastTooltip "Set working directory" $ iconButton remixFolderUserFill OpenSetWorkingDir
              `styleBasic` [bgColor transparent, border 1 transparent, padding 4, textSize u]
              `styleHover` [bgColor hoverColor]

            -- let cep = (CreateEmptyProof $ model ^. newFileName) in
            --   popup_ newFilePopupOpen [popupAlignToWindow, alignCenter, alignMiddle] (vstack [
            --     h2 "Create proof",
            --     spacer,
            --     span "Enter the name of your proof",
            --     spacer,
            --     firstKeystroke [("Enter", cep, True)] $ textField_ newFileName [placeholder "my_proof"],
            --     spacer,
            --     button "+ Create proof" cep
            --   ] `styleBasic` [bgColor popupBackground, border 1 dividerColor, padding (1.5*u), width (20*u)])
          ]) `styleBasic` [borderB 1 dividerColor, paddingV 2, paddingH 16],

        fastVScroll $ fileTreeUI parts 1
      ] `styleBasic` [ width (model ^. preferences . fileExplorerWidth), borderR 1 dividerColor ]
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
        Just wd -> (model ^. preferences . currentFile) == Just (wd </> filePath)
      ext = takeExtension filePath
      iconIdent
        | ext == ".md" = remixMarkdownFill
        | ext == "." <> feFileExt = remixBracesFill --remixSurveyFill
        | otherwise = remixMenu2Line
      iconColor
        | ext == ".md" = Just $ rgb 94 156 255
        | ext == "." <> feFileExt = Just $ rgb 255 130 0 --Just $ rgb 255 130 0
        | otherwise = Nothing

  editWindow :: WidgetNode AppModel AppEvent
  editWindow = vstack [
      fileNavBar (model ^. preferences . openFiles),
      tabWindow (model ^. preferences . currentFile)
    ]

  fileNavBar filePaths = fastHScroll (hstack (map boxedLabel filePaths))
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
            displayName = if isTemp then "Untitled proof" else pack $ takeFileName filePath
            closeText = if isFileEdited file then "●" else "⨯"
            file = getProofFileByPath (model ^. preferences . tmpLoadedFiles) filePath
            isCurrent = (model ^. preferences . currentFile) == Just filePath
            isTemp = "/_tmp/" `isInfixOf` filePath

  tabWindow :: Maybe FilePath -> WidgetNode AppModel AppEvent
  tabWindow Nothing = vstack [] `styleBasic` [expandWidth 1000] -- Don't know how expandWith works, but it works
  tabWindow (Just fileName) = case file of
    Nothing -> span ("Filepath \"" <> pack fileName <> "\" not loaded in: " <> pack (show (model ^. preferences . tmpLoadedFiles)))
    Just (PreferenceFile _ _) -> renderPreferenceTab
    Just file@(ProofFile {}) -> renderProofTab file ((pack . takeBaseName . _path) file)
    Just file@(TemporaryProofFile {}) -> renderProofTab file "[Untitled proof]"
    Just (MarkdownFile _p content) -> renderMarkdownTab content
    Just (OtherFile p content) -> renderOtherTab p content
    where file = getProofFileByPath (model ^. preferences . tmpLoadedFiles) fileName

  renderOtherTab :: FilePath -> Text -> WidgetNode AppModel AppEvent
  renderOtherTab path content = fastVScroll $ vstack_ [childSpacing] [
      label $ pack path <> ": This file type is not supported",
      paragraph content
    ] `styleBasic` [padding u]

  renderMarkdownTab :: Text -> WidgetNode AppModel AppEvent
  renderMarkdownTab content = fastVScroll (renderMarkdown model content `styleBasic` [padding u, maxWidth 300]) `nodeKey` "markdownScroll"

  renderPreferenceTab :: WidgetNode AppModel AppEvent
  renderPreferenceTab = hstack [
    vstack [
        h3 "App scale",
        hslider_ (preferences . fontSize) 8 32 [thumbVisible],
        button "Reset scale" ResetFontSize,
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
        label "I think ⊢ I am" `styleBasic` [textFont $ fromString $ model ^. preferences . logicFont],
        spacer, spacer,

        h3 "Theme",
        normalStyle $ button "Switch light/dark mode" SwitchTheme
      ] `styleBasic` [maxWidth 1024]
    ] `styleBasic` [padding 30]
    where illustThickness fontThicknessess = vstack [label "This is how thick I am" `styleBasic` [textFont $ fromString fontThicknessess, textSize u]]

  renderProofTab :: File -> Text -> WidgetNode AppModel AppEvent
  renderProofTab file heading = case parsedSequent of
    Nothing -> vstack [
        vstack [
          h1 $ pack $ _path file,
          spacer
        ] `styleBasic` [padding 10, borderB 1 dividerColor],
        fastVScroll (vstack [
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
        fastScroll (proofTreeUI parsedSequent) `styleBasic` [padding 10],
        hstack [
          proofStatusLabel,
          filler,
          button "Save proof" (SaveFile file) `styleBasic` [textSize u],
          spacer,
          button "Check proof" (CheckProof file) `styleBasic` [textSize u],
          spacer,
          button "Export to LaTeX" ExportToLaTeX 
            `styleBasic` [padding 5]
            `nodeKey` "ExportSuccess"
            `nodeKey` "ExportError",
          spacer
        ] `styleBasic` [padding 10, borderT 1 dividerColor]
      ]
      where
        subheading
          | null premises && conclusion == "" = span "Empty proof"
          | otherwise = symbolSpan prettySequent
        prettySequent = intercalate ", " premises <> " ⊢ " <> conclusion
        conclusion = replaceSpecialSymbols (_conclusion parsedSequent)
        premises = map replaceSpecialSymbols (_premises parsedSequent)
    where
      parsedSequent = _parsedSequent file

  proofStatusLabel = case model ^. proofStatus of
    Nothing -> span "Checking proof..." `styleBasic` [textColor orange]
    Just (FEError _warns error) -> span ("Proof is incorrect: " <> (pack . show) error) `styleBasic` [textColor red]
    Just (FEOk _warns) -> span "Proof is correct :)" `styleBasic` [textColor lime]

  proofTreeUI :: FESequent -> WidgetNode AppModel AppEvent
  proofTreeUI sequent = vstack [
      vstack_ [childSpacing] [
        h2 "Premises",
        vstack_ [childSpacing] $ zipWith premiseLine (_premises sequent) [0..],
        widgetIf (null $ _premises sequent) (span "No premises")
      ],
      spacer,
      hstack [button "+ Premise" (AddPremise (nrPremises - 1)) `styleBasic` [textSize u]],
      spacer, spacer,

      vstack_ [childSpacing] [
        h2 "Conclusion",
        spacer,
        box_ [alignLeft] (
          symbolStyle $ textFieldV_ (replaceSpecialSymbols (_conclusion sequent)) EditConclusion [placeholder "Enter conclusion here"]
            `styleBasic` [maxWidth 600]
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
      nrPremises = length (_premises sequent)
      premiseLine premise idx = box_ [alignLeft] (hstack [
          firstKeystroke [
            ("Up", FocusOnKey $ WidgetKey ("premise.input." <> showt (idx - 1)), True),
            ("Down", FocusOnKey $ WidgetKey ("premise.input." <> showt (idx + 1)), True),
            ("Delete", RemovePremise idx, True),
            ("Ctrl-Enter", AddPremise idx, True)
          ] (symbolStyle $ textFieldV_ (replaceSpecialSymbols premise) (EditPremise idx) [placeholder "Enter premise"]
            `nodeKey` ("premise.input." <> showt idx)),
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
              vstack [] `styleBasic` [width 250]
            ] `styleBasic` [height 34]
            where pp = replaceSpecialSymbols premise

      pf :: FEStep -> Integer -> FormulaPath -> (WidgetNode AppModel AppEvent, Integer)
      pf (SubProof p) index path = (ui, lastIndex)
        where
          ui = vstack [
              vstack_ [childSpacing] (map fst s)
                `styleBasic` [border 1 proofBoxColor, styleIf isError (border 1 red), borderR 0 transparent, paddingV 8, paddingL 24],

              widgetIf isError ((span . pack . extractErrorMsg) (model ^. proofStatus))
                `styleBasic` [textColor red, paddingT (0.5*u)]
            ]
          isError = isErrorSubProof rStart rEnd (model ^. proofStatus)
          rStart = index + toInteger (length (_premises sequent))
          rEnd = lastIndex - 1 + toInteger (length (_premises sequent))
          lastIndex = if null s then index else snd $ last s
          s = getSubProof p path 0 index

      pf (Line statement rule usedArguments arguments) index path = (ui, lastIndex)
        where
          ui = vstack [
              hstack [
                hstack [
                  -- span (pack (show (pForm (myLexer (unpack (replaceSpecialSymbolsInverse statement)))))),
                  -- span (replaceSpecialSymbolsInverse statement),

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
                    ("Ctrl-Enter", InsertLineAfter path, not isLastLine || not nextIndexExists),
                    ("Ctrl-Enter", InsertLineAfter pathToParentSubProof, isLastLine),
                    ("Enter", NextFocus 1, True)
                  ] (symbolStyle $ textFieldV_ (replaceSpecialSymbols statement) (EditFormula path) [onKeyDown handleFormulaKey, placeholder "Empty statement"]
                    `styleBasic` [styleIf isStatementError (border 1 red)]
                    `nodeKey` (showt index <> ".statement"))
                      `nodeKey` (showt index <> ".statement.keystroke"),

                  spacer,

                  hstack_ [childSpacing] [
                    firstKeystroke [
                      ("Up", FocusOnKey $ WidgetKey (showt (index - 1) <> ".rule"), prevIndexExists),
                      ("Down", FocusOnKey $ WidgetKey (showt (index + 1) <> ".rule"), nextIndexExists),
                      -- ("Left", FocusOnKey $ WidgetKey (showt index <> ".statement"), True),

                      ("Ctrl-Tab", SwitchLineToSubProof path (WidgetKey $ showt index <> ".rule"), True),
                      ("Ctrl-Shift-Tab", SwitchSubProofToLine pathToParentSubProof (WidgetKey $ showt index <> ".rule"), True),
                      ("Delete", RemoveLine path, trashActive),
                      ("Backspace", FocusOnKey (WidgetKey (showt index <> ".statement")), rule == ""),
                      ("Ctrl-Enter", InsertLineAfter path, not isLastLine || not nextIndexExists),
                      ("Ctrl-Enter", InsertLineAfter pathToParentSubProof, isLastLine),
                      ("Enter", NextFocus 1, usedArguments > 0),
                      ("Enter", InsertLineAfter path, usedArguments == 0)
                      -- ("Enter", InsertLineAfter path, True)
                    ] (symbolStyle $ textFieldV_ (replaceSpecialSymbols rule) (EditRuleName path) [onKeyDown handleRuleNameKey, placeholder "No rule"]
                      `styleBasic` [styleIf isRuleError (border 1 red)]
                      `nodeKey` (showt index <> ".rule"))
                        `nodeKey` (showt index <> ".rule.keystroke"),
                    argInputs
                  ]
                    `styleBasic` [width 300]

                  -- , textFieldSuggestionsV rule (\_i t -> EditLine path 1 t) allRules (const $ textFieldV (replaceSpecialSymbols rule) (EditLine path 1)) label `styleHover` [bgColor transparent]
                ],
                spacer,
                b
              ],

              widgetIf isError ((span . pack . extractErrorMsg) (model ^. proofStatus))
                `styleBasic` [textColor red, paddingT (0.5*u)]
            ] `nodeKey` showt index

          argInputs = widgetIf (usedArguments /= 0) $ hstack_ [childSpacing] (zipWith argInput (take usedArguments arguments) [0..])
          argInput argument idx = firstKeystroke [
              ("Up", FocusOnKey $ WidgetKey (showt (index - 1) <> ".ruleArg." <> showt idx), prevIndexExists),
              ("Down", FocusOnKey $ WidgetKey (showt (index + 1) <> ".ruleArg." <> showt idx), nextIndexExists),
              ("Ctrl-Tab", SwitchLineToSubProof path (WidgetKey $ showt index <> ".ruleArg." <> showt idx), True),
              ("Ctrl-Shift-Tab", SwitchSubProofToLine pathToParentSubProof (WidgetKey $ showt index <> ".ruleArg." <> showt idx), True),
              ("Delete", RemoveLine path, trashActive),
              ("Backspace", FocusOnKey (WidgetKey (showt index <> ".rule")), isFirstArg && argument == ""),
              ("Backspace", FocusOnKey (WidgetKey (showt index <> ".ruleArg." <> showt (idx - 1))), not isFirstArg && argument == ""),
              ("Ctrl-Enter", InsertLineAfter path, not isLastLine || not nextIndexExists),
              ("Ctrl-Enter", InsertLineAfter pathToParentSubProof, isLastArg && isLastLine),
              ("Enter", InsertLineAfter path, isLastArg),
              ("Enter", NextFocus 1, not isLastArg)
            ] (symbolStyle $ textFieldV_ (replaceSpecialSymbols argument) (EditRuleArgument path idx) [onKeyDown (handleRuleArgKey idx), placeholder ("Arg. " <> showt (index + 1)), selectOnFocus]
            `nodeKey` (showt index <> ".ruleArg." <> showt idx)
            `styleBasic` [width 70]
            `styleBasic` [styleIf isRuleArgError (border 1 red)])
            where
              isFirstArg = idx == 0
              isLastArg = idx + 1 == usedArguments
              isRuleArgError = isError

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
              ] `styleBasic` [width 250]

          -- allRules = [replaceSpecialSymbols rule] ++ (filter (\f -> (replaceSpecialSymbols . toLower) rule `isInfixOf` toLower f) $ map (pack . fst) (Data.Map.toList $ rules newEnv))--[model ^. userLens, "Thecoder", "another", "bruh", "tesdt", "dsjhnsifhbsgfsghffgusgfufgssf", "1", "2"]
          --   where rules (Env _ _ r _ _ _ _ _) = r

          handleFormulaKey, handleRuleNameKey :: (KeyMod, KeyCode, InputFieldState Text) -> AppEvent
          handleFormulaKey (_mod, code, state)
            | isKeyRight code && isAtEnd = FocusOnKey $ WidgetKey (showt index <> ".rule")
            | otherwise = NoEvent
            where
              isAtEnd = cursorPos == textLen
              cursorPos = _ifsCursorPos state
              textLen = Data.Text.length (_ifsCurrText state)

          handleRuleNameKey (_mod, code, state)
            | isKeyLeft code && isAtBeginning = FocusOnKey $ WidgetKey (showt index <> ".statement")
            | isKeyRight code && isAtEnd = FocusOnKey $ WidgetKey (showt index <> ".ruleArg.0")
            | otherwise = NoEvent
            where
              isAtEnd = cursorPos == textLen
              textLen = Data.Text.length (_ifsCurrText state)
              isAtBeginning = cursorPos == 0
              cursorPos = _ifsCursorPos state

          handleRuleArgKey :: Int -> (KeyMod, KeyCode, InputFieldState Text) -> AppEvent
          handleRuleArgKey argIdx (_mod, code, state)
            | isKeyLeft code && isAtBeginning && argIdx == 0 = FocusOnKey $ WidgetKey (showt index <> ".rule")
            | isKeyLeft code && isAtBeginning && argIdx /= 0 = FocusOnKey $ WidgetKey (showt index <> ".ruleArg." <> showt (argIdx - 1))
            | isKeyRight code && isAtEnd && argIdx + 1 < usedArguments = FocusOnKey $ WidgetKey (showt index <> ".ruleArg." <> showt (argIdx + 1))
            | otherwise = NoEvent
            where
              isAtEnd = cursorPos == textLen
              textLen = Data.Text.length (_ifsCurrText state)
              isAtBeginning = cursorPos == 0
              cursorPos = _ifsCursorPos state

          isStatementError = isError || isLeft (pForm (myLexer (unpack (replaceSpecialSymbolsInverse statement))))
          isRuleError = isError || (rule /= "" && isNothing (Data.Map.lookup rule ruleMetaDataMap))
          isError = isErrorLine lineNumber (model ^. proofStatus)
          lineNumber = index + toInteger (length (_premises sequent))
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

      ln (Line {}) index _path = (ui, lastIndex)
        where
          ui = vstack [
              symbolSpan (showt index <> ".")
                `styleBasic` [width 48, paddingR 12, height 34, textRight, styleIf isError (textColor red)]
                `nodeKey` showt index <> "label",

              widgetIf isError (span " ")
                `styleBasic` [textColor red, paddingT (0.5*u)]
            ]
          lastIndex = index + 1
          isError = isErrorLine (toInteger index) (model ^. proofStatus)

      getSubProof2 p path arrayIndex visualIndex
        | arrayIndex < length p = u : getSubProof2 p path (arrayIndex + 1) (snd u)
        | otherwise = []
          where u = ln (p !! arrayIndex) visualIndex (path ++ [arrayIndex])

  ruleSidebar = widgetIf (model ^. preferences . rulesSidebarOpen) $ fastVScroll (vstack [
      h2 "Rules" `styleBasic` [padding u],

      subsection "Propositional Logic",
      vstack $ map (ruleItem . snd) visualRuleNames0,

      subsection "First Order Logic",
      vstack $ map (ruleItem . snd) visualRuleNames1
    ]) `styleBasic` [rangeWidth 200 300, borderL 1 dividerColor]
    where
      subsection t = box (bold (span t)) `styleBasic` [padding u]
      ruleItem r = box_ [onClick NoEvent] (symbolSpan r)
        `styleBasic` [cursorHand, padding u, borderT 1 dividerColor]
        `styleHover` [bgColor hoverColor]
        `styleActive` [bgColor selectedColor]