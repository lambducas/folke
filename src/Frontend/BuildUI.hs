{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.BuildUI (
  buildUI
) where

import Prelude hiding (span)

import Frontend.Types
import Frontend.Helper
import Frontend.Themes
import Frontend.Components.Labels
import Frontend.Components.RenderMarkdown (renderMarkdown)
import Frontend.Components.RenderProofTab (renderProofTab)

import Monomer
import qualified Monomer.Lens as L
import Control.Lens
import Data.Text (Text, pack, intercalate, splitOn, toLower)
import qualified Data.Text (length)
import Data.List (sort, groupBy, isInfixOf)
import Data.Default ( Default(def) )
import Data.String (fromString)
import System.FilePath (takeExtension, takeFileName, takeBaseName)
import System.FilePath.Posix ((</>))
import Data.Maybe (fromMaybe, isJust)

import Monomer.Widgets.Containers.BoxDragToResize

menuBarCategories :: [(Text, [(Text, Text, AppEvent)])]
menuBarCategories = [
    ("File", [
      ("New Proof", "Ctrl+N", CreateEmptyProof),
      ("Save File", "Ctrl+S", SaveCurrentFile),
      ("Close File", "Ctrl+W", CloseCurrentFile),
      ("Export To LaTeX", "", ExportToLaTeX),
      ("Set Working Directory", "", OpenSetWorkingDir),
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

ctxFileExplorer :: FilePath -> FilePath -> ContextMenuActions
ctxFileExplorer filePath relativePath = [
    ("Open in File Explorer", "", OpenInExplorer filePath, True),
    -- ("Rename", "", NoEvent, False),
    ("Delete file", "Delete", OpenConfirmAction (ConfirmActionData {
      _cadTitle = "Delete file?",
      _cadBody = "Are you sure you want to delete " <> pack filePath <> "? This action cannot be undone!",
      _cadAction = DeleteFilePath filePath
    }), True),
    ("Copy Path", "", CopyToClipboard (pack filePath), True),
    ("Copy Relative Path", "", CopyToClipboard (pack relativePath), True)
  ]

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  selTheme = getActualTheme $ model ^. preferences . selectedTheme
  accentColor = selTheme ^. L.userColorMap . at "accent" . non def
  popupBackground = selTheme ^. L.userColorMap . at "popupBackground" . non def
  backgroundColor = selTheme ^. L.userColorMap . at "backgroundColor" . non def
  selectedColor = selTheme ^. L.userColorMap . at "selectedFileBg" . non def
  dividerColor = selTheme ^. L.userColorMap . at "dividerColor" . non def
  hoverColor = selTheme ^. L.userColorMap . at "hoverColor" . non def
  -- proofBoxColor = selTheme ^. L.userColorMap . at "proofBoxColor" . non def

  h1 = Frontend.Components.Labels.h1 model
  h2 = Frontend.Components.Labels.h2 model
  h3 = Frontend.Components.Labels.h3 model
  span = Frontend.Components.Labels.span model
  span_ = Frontend.Components.Labels.span_ model
  symbolSpan = Frontend.Components.Labels.symbolSpan model
  -- symbolSpan_ = Frontend.Components.Labels.symbolSpan_ model
  paragraph = Frontend.Components.Labels.paragraph model
  -- paragraph_ = Frontend.Components.Labels.paragraph_ model
  iconLabel = Frontend.Components.Labels.iconLabel model
  iconButton = Frontend.Components.Labels.iconButton model
  -- trashButton = Frontend.Components.Labels.trashButton model
  bold = Frontend.Components.Labels.bold model
  normalStyle = Frontend.Components.Labels.normalStyle model
  -- symbolStyle = Frontend.Components.Labels.symbolStyle model
  u = model ^. preferences . fontSize

  button = Frontend.Components.Labels.button model
  fastTooltip = Frontend.Components.Labels.fastTooltip model
  -- fastScroll = Frontend.Components.Labels.fastScroll
  fastVScroll = Frontend.Components.Labels.fastVScroll
  fastHScroll = Frontend.Components.Labels.fastHScroll

  globalKeybinds = filter (\(b, _, _) -> b /= "") $ map (\(_, b, e) -> (convertBind b, e, True)) $ concatMap snd menuBarCategories
    where
      convertBind b = intercalate "-" (map fixKey (splitOn "+" b))
      fixKey k
        | Data.Text.length k == 1 = toLower k
        | otherwise = k

  widgetTree =
    firstKeystroke globalKeybinds $
      themeSwitch_ selTheme [themeClearBg] $
        vstack [
          vstack [
            menuBar,
            mainContent
          ],
          contextMenuUI,
          confirmActionUI
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

  dts :: DragSide -> ALens' AppModel Double -> WidgetNode AppModel AppEvent -> WidgetNode AppModel AppEvent
  dts DragSideRight lens w = boxDragToResize_ DragSideRight lens [] $ hstack [w, spc]
    where spc = vstack [] `styleBasic` [width 10]
  dts DragSideLeft lens w = boxDragToResize_ DragSideLeft lens [] $ hstack [spc, w]
    where spc = vstack [] `styleBasic` [width 10]

  fileExplorerSidebar :: WidgetNode AppModel AppEvent
  fileExplorerSidebar = widgetIf (model ^. preferences . fileExplorerOpen) $ dts DragSideRight (preferences . fileExplorerWidth) $ case model ^. preferences . workingDir of
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
      ] `styleBasic` [ borderR 1 dividerColor ]

    Just _ -> case model ^. filesInDirectory of
      Nothing -> vstack [
          box_ [expandContent] (hstack [
              bold (span "File Explorer"),
              filler,
              fastTooltip "Set working directory" $ iconButton remixFolderUserFill OpenSetWorkingDir
                `styleBasic` [bgColor transparent, border 1 transparent, padding 4, textSize u]
                `styleHover` [bgColor hoverColor]
            ]) `styleBasic` [borderB 1 dividerColor, paddingV 2, paddingH 16],

          vstack_ [childSpacing] [
            paragraph "Error loading working directory. It might not exist.",
            box (button "Open Other Folder" OpenSetWorkingDir)
          ] `styleBasic` [padding u]
        ] `styleBasic` [ borderR 1 dividerColor ]
      Just fid -> vstack [
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
            ]) `styleBasic` [borderB 1 dividerColor, paddingV 2, paddingH 16],

          fastVScroll $ fileTreeUI parts 1
        ] `styleBasic` [ borderR 1 dividerColor ]
        where
          parts = map (\f -> (splitOn "/" (pack f), f)) files
          files = sort fid

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

  fileItem indent text filePath = box_ [expandContent, onBtnReleased handleBtn] $ hstack_ [childSpacing] [
      iconLabel iconIdent `styleBasic` [fromMaybe mempty (iconColor >>= Just . textColor)],
      span_ text [ellipsis]
    ]
      `styleHover` [styleIf (not isCurrent) (bgColor hoverColor)]
      `styleBasic` [borderB 1 dividerColor, paddingL (16 * indent), paddingR 16, paddingV 8, cursorHand, styleIf isCurrent (bgColor selectedColor)]
    where
      handleBtn BtnLeft _ = OpenFile filePath
      handleBtn BtnRight _ = OpenContextMenu (ctxFileExplorer fullPath filePath)
      handleBtn _ _ = NoEvent
      isCurrent = (model ^. preferences . currentFile) == Just fullPath
      fullPath = case model ^. preferences . workingDir of
        Nothing -> ""
        Just wd -> wd </> filePath
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
    Just file@(ProofFile {}) -> renderProofTab wenv model file ((pack . takeBaseName . _path) file)
    Just file@(TemporaryProofFile {}) -> renderProofTab wenv model file "[Untitled proof]"
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

  ruleSidebar = widgetIf (model ^. preferences . rulesSidebarOpen) $ dts DragSideLeft (preferences . rulesSidebarWidth) $ fastVScroll (vstack [
      h2 "Rules" `styleBasic` [padding u],

      subsection "Propositional Logic",
      vstack $ map (ruleItem . snd) visualRuleNames0,

      subsection "First Order Logic",
      vstack $ map (ruleItem . snd) visualRuleNames1
    ]) `styleBasic` [borderL 1 dividerColor]
    where
      subsection t = box (bold (span t)) `styleBasic` [padding u]
      ruleItem r = box_ [onClick NoEvent] (symbolSpan r)
        `styleBasic` [cursorHand, padding u, borderT 1 dividerColor]
        `styleHover` [bgColor hoverColor]
        `styleActive` [bgColor selectedColor]

  contextMenuUI = popupV_ (model ^. contextMenu . ctxOpen) (\s -> if s then NoEvent else CloseContextMenu) [popupOpenAtCursor]
    (vstack (map dropdownButton actions)
      `styleBasic` [width 300, bgColor popupBackground, border 1 dividerColor, padding 4, radius 4, textSize $ u -2])
    where
      actions = model ^. contextMenu . ctxActions
      dropdownButton (name, keybind, action, active)
        | active = box_ [onClick action, onClick CloseContextMenu, expandContent] $ hstack [
            spn name,
            filler,
            spn keybind
          ]
            `styleBasic` [radius 4, paddingV 10, paddingH 20, cursorHand]
            `styleHover` [bgColor hoverColor]
        | otherwise = box_ [expandContent] $ hstack [
            spnIna name,
            filler,
            spnIna keybind
          ]
            `styleBasic` [radius 4, paddingV 10, paddingH 20]
          where
            spn t = span t `styleBasic` [textSize $ u -2]
            spnIna t = spn t `styleBasic` [textColor dividerColor]

  confirmActionUI = popupV_ (isJust cad) (const NoEvent) [popupAlignToWindow, popupDisableClose, alignCenter, alignMiddle] (vstack_ [childSpacing] [
      h1 title,
      paragraph body,
      spacer,
      hstack_ [childSpacing] [
        normalStyle $ button "Cancel" (CloseConfirmAction NoEvent),
        normalStyle $ button "Confirm" (CloseConfirmAction action)
      ]
    ] `styleBasic` [bgColor popupBackground, border 1 dividerColor, padding 20, textSize $ u -2])
    where
      title = fromMaybe "" (cad >>= Just . _cadTitle)
      body = fromMaybe "" (cad >>= Just . _cadBody)
      action = fromMaybe NoEvent (cad >>= Just . _cadAction)
      cad = model ^. confirmActionPopup