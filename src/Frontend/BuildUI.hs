{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.BuildUI (
  buildUI
) where

import Prelude hiding (span)

import Frontend.Types
import Frontend.Helper.General
import Frontend.Themes
import Frontend.Components.GeneralUIComponents
import Frontend.Components.RenderMarkdown (renderMarkdown)
import Frontend.Components.RenderProofTab (renderProofTab)
import Frontend.Components.Details

import Monomer
import qualified Monomer.Lens as L
import Control.Lens
import Data.Text (Text, pack, intercalate, splitOn, toLower)
import qualified Data.Text (length)
import Data.List (sort, isInfixOf)
import Data.Default ( Default(def) )
import Data.String (fromString)
import System.FilePath (takeFileName, takeBaseName)
import Data.Maybe (fromMaybe, isJust)

menuBarCategories :: [(Text, [(Text, Text, AppEvent)])]
menuBarCategories = [
    ("File", [
      ("New Proof", "Ctrl+N", CreateEmptyProof),
      ("Save File", "Ctrl+S", SaveCurrentFile),
      ("Open File", "Ctrl+O", OpenFileFromFileSystem),
      ("Close File", "Ctrl+W", CloseCurrentFile),
      ("Export To LaTeX", "", ExportToLaTeX),
      ("Export to LaTeX and PDF", "", ExportToPDF),
      ("Set Working Directory", "", OpenSetWorkingDir),
      ("Exit", "", ExitApp)
    ]),
    ("Edit", [
      ("Undo", "Ctrl+Z", Undo),         
      ("Redo", "Ctrl+Y", Redo),      
      -- ("Make Subproof", "Ctrl+Tab", NoEvent),
      -- ("Undo Subproof", "Ctrl+Shift+Tab", NoEvent),
      -- ("Goto Next Input", "Return", NoEvent),
      -- ("Insert Line Below", "Ctrl+Enter", NoEvent),
      -- ("Close Subproof", "Ctrl+Enter", NoEvent),
      ("Validate Proof", "Ctrl+R", CheckCurrentProof)
    ]),
    ("View", [
      ("Toggle File Explorer", "Ctrl+B", ToggleFileExplorer),
      ("Toggle Rules Dictionary", "", ToggleRulesSidebar),
      ("Open Preferences", "Ctrl+Shift+P", OpenPreferences)
    ]),
    ("Help", [
      ("Open Welcome Page", "", OpenWelcome),
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

  h1 = Frontend.Components.GeneralUIComponents.h1 model
  h2 = Frontend.Components.GeneralUIComponents.h2 model
  h3 = Frontend.Components.GeneralUIComponents.h3 model
  span = Frontend.Components.GeneralUIComponents.span model
  -- span_ = Frontend.Components.GeneralUIComponents.span_ model
  symbolSpan = Frontend.Components.GeneralUIComponents.symbolSpan model
  -- symbolSpan_ = Frontend.Components.GeneralUIComponents.symbolSpan_ model
  paragraph = Frontend.Components.GeneralUIComponents.paragraph model
  -- paragraph_ = Frontend.Components.GeneralUIComponents.paragraph_ model
  -- iconLabel = Frontend.Components.GeneralUIComponents.iconLabel model
  iconButton = Frontend.Components.GeneralUIComponents.iconButton model
  -- trashButton = Frontend.Components.GeneralUIComponents.trashButton model
  bold = Frontend.Components.GeneralUIComponents.bold model
  normalStyle = Frontend.Components.GeneralUIComponents.normalStyle model
  -- symbolStyle = Frontend.Components.GeneralUIComponents.symbolStyle model
  u = model ^. preferences . fontSize

  button = Frontend.Components.GeneralUIComponents.button model
  fastTooltip = Frontend.Components.GeneralUIComponents.fastTooltip model
  -- fastScroll = Frontend.Components.GeneralUIComponents.fastScroll
  fastVScroll = Frontend.Components.GeneralUIComponents.fastVScroll
  fastHScroll = Frontend.Components.GeneralUIComponents.fastHScroll

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
      hsplit_ [secondIsMain, splitIgnoreChildResize True, splitHandlePos (persistentState . rulesSidebarWidth)] (
        hsplit_ [firstIsMain, splitIgnoreChildResize True, splitHandlePos (persistentState . fileExplorerWidth)] (
          fileExplorerSidebar,
          editWindow
        ),
        ruleSidebar
      )
    ] `styleBasic` [expandHeight 100000]

  actionSidebar :: WidgetNode AppModel AppEvent
  actionSidebar = vstack (map actionButton [
      ("Toggle File Explorer (Ctrl+B)", remixFileSearchLine, ToggleFileExplorer, model ^. persistentState . fileExplorerOpen),
      ("Toggle Rule Explorer", remixRuler2Line, ToggleRulesSidebar, model ^. persistentState . rulesSidebarOpen),
      ("Open Preferences", remixSettings4Line, OpenPreferences, False),
      ("Open Guide", remixQuestionLine, OpenGuide, False)
    ]) `styleBasic` [width 50, borderR 1 dividerColor]
    where
      actionButton (tt, icon, event, selected) = box_ [alignCenter, alignMiddle] btn `styleBasic` [height 50]
        where
          btn = fastTooltip tt $ iconButton icon event
            `styleHover` [bgColor hoverColor]
            `styleBasic` [radius (0.5*u), styleIf selected (textColor accentColor), styleIf selected (bgColor selectedColor)]

  -- dts :: DragSide -> ALens' AppModel Double -> WidgetNode AppModel AppEvent -> WidgetNode AppModel AppEvent
  -- dts DragSideRight lens w = boxDragToResize_ DragSideRight lens [] $ hstack [w, spc]
  --   where spc = vstack [] `styleBasic` [width 10]
  -- dts DragSideLeft lens w = boxDragToResize_ DragSideLeft lens [] $ hstack [spc, w]
  --   where spc = vstack [] `styleBasic` [width 10]

  fileExplorerSidebar :: WidgetNode AppModel AppEvent
  fileExplorerSidebar =
    widgetIf (model ^. persistentState . fileExplorerOpen) $
      case model ^. persistentState . workingDir of
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
          ] `styleBasic` [ borderR 1 dividerColor, rangeWidth 200 1000 ]

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
            ] `styleBasic` [ borderR 1 dividerColor, rangeWidth 200 1000 ]
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

              fastVScroll $ details [DetailsCfg {
                    _dcOnOpenFile = [RaiseEvent . OpenFile],
                    _dcOnOpenContextMenu = [\g l -> RaiseEvent $ OpenContextMenu (ctxFileExplorer g l)]
                  }] model parts
            ] `styleBasic` [ borderR 1 dividerColor, rangeWidth 200 1000 ]
            where
              parts = map (\f -> (splitOn "/" (pack f), f)) files
              files = sort fid

  editWindow :: WidgetNode AppModel AppEvent
  editWindow = vstack [
      fileNavBar (model ^. persistentState . openFiles),
      tabWindow (model ^. persistentState . currentFile)
    ]

  fileNavBar filePaths = fastHScroll (hstack (zipWith renderTabHandle filePaths [0..]))
    `styleBasic` [bgColor selectedColor, maxHeight 40, minHeight 40, height 40]
    where
      renderTabHandle filePath idx = dt $ dg $ box_ [expandContent, onClick (SetCurrentFile filePath)] $ hstack [
          spacer,
          fastTooltip (pack filePath) $ span displayName,
          spacer,
          fastTooltip "Close tab" $
            box_ [onClick (CloseFile filePath)] (symbolSpan closeText
              `styleBasic` [textSize (1.5*u), radius 8, padding 4]
              `styleHover` [bgColor hoverColor])
        ]
          `styleBasic` [borderR 1 dividerColor, styleIf isCurrent (bgColor backgroundColor), cursorHand, paddingH 4]
          `styleHover` [styleIf (not isCurrent) (bgColor hoverColor)]
          where
            displayName = if isTemp then "Untitled proof" else pack $ takeFileName filePath
            closeText = if isFileEdited file then "●" else "⨯"
            file = getProofFileByPath (model ^. persistentState . tmpLoadedFiles) filePath
            isCurrent = (model ^. persistentState . currentFile) == Just filePath
            isTemp = "/_tmp/" `isInfixOf` filePath

            dt = dropTarget_ (\from -> MoveTab from idx) [dropTargetStyle hoverStyle]
            hoverStyle = [borderR 3 accentColor]

            dg = draggable_ idx [draggableStyle dragStyle]
            dragStyle = [bgColor hoverColor]

  tabWindow :: Maybe FilePath -> WidgetNode AppModel AppEvent
  tabWindow Nothing = vstack [] `styleBasic` [expandWidth 1000] -- Don't know how expandWith works, but it works
  tabWindow (Just fileName) = case file of
    Nothing -> span ("Filepath \"" <> pack fileName <> "\" not loaded in: " <> pack (show (model ^. persistentState . tmpLoadedFiles)))
    Just (PreferenceFile _ _) -> renderPreferenceTab
    Just file@(ProofFile {}) -> renderProofTab wenv model file ((pack . takeBaseName . _path) file)
    Just file@(TemporaryProofFile {}) -> renderProofTab wenv model file "[Untitled proof]"
    Just (MarkdownFile _p content) -> renderMarkdownTab content
    Just (OtherFile p content) -> renderOtherTab p content
    where file = getProofFileByPath (model ^. persistentState . tmpLoadedFiles) fileName

  renderOtherTab :: FilePath -> Text -> WidgetNode AppModel AppEvent
  renderOtherTab path content = fastVScroll $ vstack_ [childSpacing] [
      label $ pack path <> ": This file type is not supported",
      paragraph content
    ] `styleBasic` [padding u]

  renderMarkdownTab :: Text -> WidgetNode AppModel AppEvent
  renderMarkdownTab content = fastVScroll (renderMarkdown wenv model content `styleBasic` [padding u, maxWidth 300]) `nodeKey` "markdownScroll"

  renderPreferenceTab :: WidgetNode AppModel AppEvent
  renderPreferenceTab = hstack [
      vstack [
        h3 "App scale",
        paragraph "The application needs to be restarted before changes take effect",
        spacer,
        hstack [
          symbolSpan (showDecimals 2 $ model ^. preferences . appScale),
          hslider_ (preferences . appScale) 0.25 3 [thumbVisible]
        ],
        spacer,
        button "Reset scale" ResetAppScale,
        spacer, spacer,

        h3 "Font size",
        spacer,
        hstack [
          symbolSpan (showDecimals 0 $ model ^. preferences . fontSize),
          hslider_ (preferences . fontSize) 8 32 [thumbVisible]
        ],
        spacer,
        button "Reset font size" ResetFontSize,
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
        normalStyle $ button "Switch light/dark mode" SwitchTheme,
        spacer, spacer,

        h3 "Extra character shortcuts",
        paragraph "Replace A and E in formula fields",
        labeledCheckbox "Enabled" (preferences . replaceAEInFormula),
        spacer, spacer

      ] `styleBasic` [maxWidth 1024]
    ] `styleBasic` [padding 30]
    where illustThickness fontThicknessess = vstack [label "This is how thick I am" `styleBasic` [textFont $ fromString fontThicknessess, textSize u]]

  ruleSidebar = widgetIf (model ^. persistentState . rulesSidebarOpen) $ fastVScroll (vstack [
      h2 "Rules" `styleBasic` [padding u],

      subsection "Propositional Logic",
      vstack $ map (ruleItem . snd) visualRuleNames0,

      subsection "First Order Logic",
      vstack $ map (ruleItem . snd) visualRuleNames1
    ]) `styleBasic` [borderL 1 dividerColor, rangeWidth 100 500]
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

-- | Converts a list of font styles for a given font to a readable name
fontListToText :: [String] -> Text
fontListToText fontList | head fontList == "Regular" = "Default"
                        | head fontList == "Roboto_Regular" = "Roboto"
                        | head fontList == "Comic_Sans_Thin" = "Comic Sans"
                        | head fontList == "Dyslexic" = "Dyslexic"
                        | otherwise = "forgor_to_label"