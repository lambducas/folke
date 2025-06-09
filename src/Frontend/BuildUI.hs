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
import Data.Text (Text, pack, intercalate, splitOn, toLower, isInfixOf, unpack)
import qualified Data.Text (length)
import Data.Default ( Default(def) )
import Data.String (fromString)
import System.FilePath (takeFileName, takeBaseName, makeRelative)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Map
import TextShow (showt)
import Frontend.Helper.ProofHelper (getCurrentFEDocument, editNameInUDR)

menuBarCategories :: Bool -> [(Text, [(Text, Text, AppEvent)])]
menuBarCategories isMac = [
    ("File", [
      ("New Proof", ctrl <> "+N", CreateEmptyProof),
      ("Save Proof", ctrl <> "+S", SaveCurrentFile),
      ("Open Proof", ctrl <> "+O", OpenFileFromFileSystem),
      ("Open Example", "", OpenFileExample),
      ("Close Tab", "Ctrl+W", CloseCurrentFile),
      ("Export proof", "", SetExportOpen True),
      ("Set Working Directory", "", OpenSetWorkingDir),
      ("Exit", "", ExitApp)
    ]),
    ("Edit", [
      ("Undo", ctrl <> "+Z", Undo),
      ("Redo", ctrl <> "+Y", Redo),
      -- ("Make Subproof", ctrl <> "+Tab", NoEvent),
      -- ("Undo Subproof", ctrl <> "+Shift+Tab", NoEvent),
      -- ("Goto Next Input", "Return", NoEvent),
      -- ("Insert Line Below", ctrl <> "+Enter", NoEvent),
      -- ("Close Subproof", ctrl <> "+Enter", NoEvent),
      ("Validate Proof", ctrl <> "+R", CheckCurrentProof),
      ("User Defined Rules", "", OpenUDR)
    ]),
    ("View", [
      ("Toggle File Explorer", ctrl <> "+B", ToggleFileExplorer),
      ("Toggle Rules Dictionary", ctrl <> "+Alt+B", ToggleRulesSidebar),
      ("Open Preferences", ctrl <> "+Shift+P", OpenPreferences),
      ("Search for File", ctrl <> "+P", OpenFileSearcher)
    ]),
    ("Help", [
      ("Open Welcome Page", "", OpenWelcome),
      ("Open Guide", "", OpenGuide),
      ("About Folke", "", OpenAbout)
    ])
  ]
  where ctrl = if isMac then "Cmd" else "Ctrl"

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
  :: Text
  -> WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI os wenv model = widgetTree where
  isMac = os == "Mac OS X"
  ctrl = if isMac then "Cmd" else "Ctrl"

  selTheme = getActualTheme $ model ^. preferences . selectedTheme
  clearColor = selTheme ^. L.clearColor
  accentColor = selTheme ^. L.userColorMap . at "accent" . non def
  popupBackground = selTheme ^. L.userColorMap . at "popupBackground" . non def
  backgroundColor = selTheme ^. L.userColorMap . at "backgroundColor" . non def
  selectedColor = selTheme ^. L.userColorMap . at "selectedFileBg" . non def
  dividerColor = selTheme ^. L.userColorMap . at "dividerColor" . non def
  hoverColor = selTheme ^. L.userColorMap . at "hoverColor" . non def
  proofBoxColor = selTheme ^. L.userColorMap . at "rSCproofBoxColor" . non def

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
  -- iconToggleButton = Frontend.Components.GeneralUIComponents.iconToggleButton model
  iconToggleButton_ = Frontend.Components.GeneralUIComponents.iconToggleButton_ model
  trashButton = Frontend.Components.GeneralUIComponents.trashButton model
  bold = Frontend.Components.GeneralUIComponents.bold model
  normalStyle = Frontend.Components.GeneralUIComponents.normalStyle model
  -- symbolStyle = Frontend.Components.GeneralUIComponents.symbolStyle model
  button = Frontend.Components.GeneralUIComponents.button model
  fastTooltip = Frontend.Components.GeneralUIComponents.fastTooltip model
  -- fastScroll = Frontend.Components.GeneralUIComponents.fastScroll
  fastVScroll = Frontend.Components.GeneralUIComponents.fastVScroll
  fastVScroll_ = Frontend.Components.GeneralUIComponents.fastVScroll_
  -- fastHScroll = Frontend.Components.GeneralUIComponents.fastHScroll
  fastHScroll_ = Frontend.Components.GeneralUIComponents.fastHScroll_
  boxShadow = Frontend.Components.GeneralUIComponents.boxShadow

  u = model ^. preferences . fontSize

  globalKeybinds = filter (\(b, _, _) -> b /= "") $ map (\(_, b, e) -> (convertBind b, e, True)) $ concatMap snd (menuBarCategories isMac)
    where
      convertBind b = intercalate "-" (map fixKey (splitOn "+" b))
      fixKey k
        | Data.Text.length k == 1 = toLower k
        | otherwise = k

  widgetTree =
    firstKeystroke globalKeybinds $
      vstack [
        -- Hack to force-merge widgets so theme updates correctly
        widgetIf (selTheme == customLightTheme) (label "" `nodeVisible` False),
        themeSwitch_ selTheme [themeClearBg] $
          vstack [
            vstack [
              menuBar,
              toolbar,
              mainContent
            ],
            userDefinedRuleUI,
            ruleGuideUI,
            fileSearcherUI,
            exportOptionsUI,
            contextMenuUI,
            confirmActionUI
          ]
      ]

  menuBar = hstack [
      menuBarPopup,
      categoryUI
    ] `styleBasic` [borderB 1 dividerColor]
    where
      catButtonWidth = 60
      categoryUI = hstack (zipWith menuBarButton (menuBarCategories isMac) [0..])
        `styleBasic` [padding 5, textSize $ u -2]

      menuBarPopup :: WidgetNode AppModel AppEvent
      menuBarPopup = popupV_ (isJust $ model ^. openMenuBarItem) (\s -> if s then NoEvent else SetOpenMenuBarItem Nothing) [] $
        vstack [
          categoryUI `styleBasic` [bgColor clearColor],
          box (
            box_ [onClickEmpty (SetOpenMenuBarItem Nothing)] (
              boxShadow $ vstack (map dropdownButton actions)
                `styleBasic` [width 300, bgColor popupBackground, border 1 dividerColor, padding 4, radius 4, textSize $ u -2]
            )
              `styleBasic` [paddingL offset]
          )
            `styleBasic` [paddingT (-20), paddingL (-19)]
        ]
        where
          actions = fromMaybe [] $ (model ^. openMenuBarItem) >>= \idx -> Just $ snd $ menuBarCategories isMac !! fromInteger idx
          offset = catButtonWidth * fromIntegral (fromMaybe 0 (model ^. openMenuBarItem))

      menuBarButton (name, _actions) idx = vstack [
          box_ (
            [onClick (SetOpenMenuBarItem (Just idx)) | isNothing (model ^. openMenuBarItem)] ++
            [onEnter (SetOpenMenuBarItem (Just idx)) | isJust $ model ^. openMenuBarItem] ++
            [onClick (SetOpenMenuBarItem Nothing) | isJust $ model ^. openMenuBarItem]
          ) (
            box (
              span name
                `styleBasic` [textSize $ u -2]
            )
              `styleBasic` [radius 4, paddingV 5, paddingH 10]
              `styleBasic` [width catButtonWidth]
              `styleHover` [bgColor selectedColor]
          )
        ]

      dropdownButton (name, keybind, action) = box_ [onClick action, onClick (SetOpenMenuBarItem Nothing), expandContent] $ hstack [
          span name `styleBasic` [textSize $ u -2],
          filler,
          span keybind `styleBasic` [textSize $ u -2]
        ]
          `styleBasic` [radius 4, paddingV 10, paddingH 20, cursorHand, textSize $ u -2]
          `styleHover` [bgColor hoverColor]

  toolbar = hstack_ [childSpacing] [
      fastTooltip ("New proof (" <> ctrl <> "+N)") $ iconButton remixFileAddLine CreateEmptyProof,
      fastTooltip ("Open proof (" <> ctrl <> "+O)") $ iconButton remixFolderOpenLine OpenFileFromFileSystem,
      fastTooltip ("Save proof (" <> ctrl <> "+S)") $ iconButton remixSave3Line SaveCurrentFile,
      fastTooltip "Export" $ iconButton remixUpload2Fill (SetExportOpen True),

      separatorLine,

      fastTooltip ("Undo (" <> ctrl <> "+Z)") $ iconButton remixArrowGoBackFill Undo,
      fastTooltip ("Redo (" <> ctrl <> "+Y)") $ iconButton remixArrowGoForwardFill Redo,

      separatorLine,

      fastTooltip ("Validate proof (" <> ctrl <> "+R)") $ iconButton remixCheckboxMultipleLine CheckCurrentProof,
      fastTooltip "Auto-validate proof" autoCheckCheck,
      fastTooltip "User defined rules" $ iconButton remixUserSettingsLine OpenUDR,

      filler,

      fastTooltip "Warning severity" warningSeverity
    ]
      `styleBasic` [padding 10, borderB 1 dividerColor]
    where
      warningSeverity = hstack_ [childSpacing] [
          span "Warnings",
          textDropdown_ (preferences . warningMessageSeverity) [3, 2, 1] intToWarningSeverity []
            `styleBasic` [width 120]
        ]

      autoCheckCheck = iconToggleButton_ remixRefreshFill (preferences . autoCheckProofTracker . acpEnabled) [onChange $ \c -> if c then CheckCurrentProof else NoEvent]
        `styleBasic` [radius 8]

  mainContent = hstack [
      actionSidebar,
      explorerEditAndRules
    ] `styleBasic` [expandHeight 100000]
    where
      explorerEditAndRules = if model ^. persistentState . rulesSidebarOpen
        then hsplit_ [secondIsMain, splitIgnoreChildResize True, splitHandlePos (persistentState . rulesSidebarWidth)] (
          explorerAndEdit,
          rulesSidebar
        )
        else explorerAndEdit

      explorerAndEdit = if model ^. persistentState . fileExplorerOpen
        then hsplit_ [firstIsMain, splitIgnoreChildResize True, splitHandlePos (persistentState . fileExplorerWidth)] (
          fileExplorerSidebar,
          editWindow `styleBasic` [borderL 1 dividerColor]
        )
        else editWindow

  actionSidebar :: WidgetNode AppModel AppEvent
  actionSidebar = vstack (map actionButton [
      ("Toggle File Explorer", remixFoldersLine, ToggleFileExplorer, model ^. persistentState . fileExplorerOpen),
      ("Toggle Rule Explorer", remixGitRepositoryLine, ToggleRulesSidebar, model ^. persistentState . rulesSidebarOpen),
      ("Open Preferences", remixSettings4Line, OpenPreferences, False),
      ("Open Guide", remixQuestionLine, OpenGuide, False)
    ]) `styleBasic` [width 50, borderR 1 dividerColor]
    where
      actionButton (tt, icon, event, selected) = box_ [alignCenter, alignMiddle] btn `styleBasic` [height 50]
        where
          btn = fastTooltip tt $ iconButton icon event
            `styleHover` [bgColor hoverColor]
            `styleBasic` [radius 8, styleIf selected (textColor accentColor), styleIf selected (bgColor selectedColor)]

  -- dts :: DragSide -> ALens' AppModel Double -> WidgetNode AppModel AppEvent -> WidgetNode AppModel AppEvent
  -- dts DragSideRight lens w = boxDragToResize_ DragSideRight lens [] $ hstack [w, spc]
  --   where spc = vstack [] `styleBasic` [width 10]
  -- dts DragSideLeft lens w = boxDragToResize_ DragSideLeft lens [] $ hstack [spc, w]
  --   where spc = vstack [] `styleBasic` [width 10]

  fileExplorerSidebar :: WidgetNode AppModel AppEvent
  fileExplorerSidebar =
    box_ [mergeRequired hasChanged, expandContent] $
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
          ] `styleBasic` [ rangeWidth 200 1000 ]

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
            ] `styleBasic` [ rangeWidth 200 1000 ]
          Just fid -> vstack [
              box_ [expandContent] (hstack [
                  bold (span "File Explorer"),
                  filler,
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
                  }] model fid
            ] `styleBasic` [ rangeWidth 200 1000 ]
    where
      hasChanged _wenv old new =
        old ^. filesInDirectory /= new ^. filesInDirectory ||
        old ^. preferences . selectedTheme /= new ^. preferences . selectedTheme ||
        old ^. persistentState . fileExplorerOpen /= new ^. persistentState . fileExplorerOpen ||
        old ^. persistentState . currentFile /= new ^. persistentState . currentFile ||
        old ^. persistentState . workingDir /= new ^. persistentState . workingDir

  editWindow :: WidgetNode AppModel AppEvent
  editWindow = vstack [
      fileNavBar (model ^. persistentState . openFiles),
      tabWindow
    ]

  fileNavBar filePaths = widgetIf (not $ null filePaths) $
    fastHScroll_ [scrollOverlay, thumbWidth 4, barWidth 4, barColor transparent] (hstack (zipWith renderTabHandle filePaths [0..]))
      `styleBasic` [bgColor selectedColor, maxHeight 40, minHeight 40, height 40]
    where
      renderTabHandle filePath idx = dt $ dg $ box_ [expandContent, onClick (SetCurrentFile filePath), onBtnReleased handleBtn] $ hstack [
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
            displayName = case file >>= _tabDisplay of
              Nothing -> pack $ takeFileName filePath
              Just d -> d
            closeText = if isFileEdited file then "●" else "⨯"
            file = getProofFileByPath (model ^. persistentState . tmpLoadedFiles) filePath
            isCurrent = (model ^. persistentState . currentFile) == Just filePath

            dt = dropTarget_ (\from -> MoveTab from idx) [dropTargetStyle hoverStyle]
            hoverStyle = [borderR 3 accentColor]

            dg = draggable_ idx [draggableStyle dragStyle]
            dragStyle = [bgColor hoverColor]

            relativePath = case model ^. persistentState . workingDir of
              Nothing -> filePath
              Just wd -> makeRelative wd filePath
            handleBtn BtnRight _ = OpenContextMenu (ctxFileExplorer filePath relativePath)
            handleBtn _ _ = NoEvent

  tabWindow = widget
    where
      widget = if null (model ^. persistentState . openFiles)
        then noTabs
        else aTab
      noTabs = vstack_ [childSpacing] [
          h2 "Folke",
          box_ [alignLeft] $ internalLink wenv model "Create a proof" CreateEmptyProof,
          box_ [alignLeft] $ internalLink wenv model "Open an example" OpenFileExample,
          box_ [alignLeft] $ internalLink wenv model "View guide" OpenGuide
        ]
        `styleBasic` [expandWidth 1000, padding 30]
      aTab = case model ^. persistentState . currentFile of
        Nothing -> vstack [] `styleBasic` [expandWidth 1000] -- Don't know how expandWith works, but it works
        (Just fileName) -> case file of
          Nothing -> span ("Filepath \"" <> pack fileName <> "\" not loaded in: " <> pack (show (model ^. persistentState . tmpLoadedFiles)))
          Just (PreferenceFile {}) -> renderPreferenceTab
          Just file@(ProofFile {}) -> renderProofTab isMac wenv model file ((pack . takeBaseName . _path) file)
          Just file@(TemporaryProofFile {}) -> renderProofTab isMac wenv model file "New proof"
          Just (MarkdownFile _p _ content) -> renderMarkdownTab content
          Just (OtherFile p _ content) -> renderOtherTab p content
          where file = getProofFileByPath (model ^. persistentState . tmpLoadedFiles) fileName

  renderOtherTab :: FilePath -> Text -> WidgetNode AppModel AppEvent
  renderOtherTab path _content = fastVScroll $ vstack_ [childSpacing] [
      label $ pack path <> ": This file type is not supported"
    ] `styleBasic` [padding u]

  renderMarkdownTab :: Text -> WidgetNode AppModel AppEvent
  renderMarkdownTab content = fastVScroll (renderMarkdown wenv model content `styleBasic` [padding (1.25*u), maxWidth 300]) `nodeKey` "markdownScroll"

  renderPreferenceTab :: WidgetNode AppModel AppEvent
  renderPreferenceTab = fastVScroll $ hstack [
      vstack_ [childSpacing] [
        h2 "App scale",
        paragraph "Change the app scale if the window content is too small or big",
        box_ [alignLeft] $ hstack [
          symbolSpan (showDecimals 2 $ model ^. preferences . appScale),
          box $ hslider_ (preferences . appScale) 0.25 3 [thumbVisible, thumbFactor 2],
          button "Reset scale" ResetAppScale
        ]
          `styleBasic` [width 500],
        paragraph "The application needs to be restarted before changes take effect",
        spacer, spacer,

        h2 "Font",
        paragraph "Global font settings",
        h3 "Size",
        box_ [alignLeft] $ hstack [
          symbolSpan (showDecimals 0 $ model ^. preferences . fontSize),
          box $ hslider_ (preferences . fontSize) 8 32 [thumbVisible, thumbFactor 2],
          button "Reset font size" ResetFontSize
        ]
          `styleBasic` [width 500],

        h3 "Family",
        box_ [alignLeft] $ textDropdown_ (preferences . selectNormalFont) [
            ["Regular","Medium","Bold"],
            ["Dyslexic"],
            ["Roboto_Regular","Roboto_Medium","Roboto_Bold"],
            ["Comic_Sans_Thin", "Comic_Sans_Regular", "Comic_Sans_Medium", "Comic_Sans_Bold"]
          ] fontListToText [onChange UpdateFont]
          `styleBasic` [textSize u, width 200],

        h3 "Weight",
        box_ [alignLeft] $ textDropdown_ (preferences . normalFont) (model ^. preferences . selectNormalFont) pack []
          `styleBasic` [textSize u, width 200],
        spacer,
        span "Preview: This is what text will look like",
        spacer, spacer,

        h3 "Symbol font",
        paragraph "The symbolic font is the font used in logic proofs",
        box_ [alignLeft] $ textDropdown_ (preferences . logicFont) ["Symbol_Regular", "Symbol_Medium", "Symbol_Bold"] pack []
          `styleBasic` [textSize u, width 200],
        spacer,
        symbolSpan "Preview: I think ⊢ I am",
        spacer, spacer,

        h3 "Theme",
        paragraph "Toggle between light and dark theme",
        box_ [alignLeft] $ normalStyle $ button toggleThemeText SwitchTheme,
        spacer, spacer,

        h3 "Extra character shortcuts",
        labeledCheckbox_ "Enable additional character shortcuts" (preferences . replaceAEInFormula) [textRight],
        paragraph "The following characters will be replaced by mathematical symbols in proofs:",
        paragraph " A -> ∀\n E -> ∃\n v -> ∨\n a -> ∧\n - -> ¬" `styleBasic` [textFont $ fromString $ model ^. preferences . logicFont],
        spacer, spacer

      ] `styleBasic` [maxWidth 1024]
    ] `styleBasic` [padding 30]
    where
      toggleThemeText = if model ^. preferences . selectedTheme == Light
        then "Light theme"
        else "Dark theme"

  rulesSidebar = box_ [mergeRequired hasChanged] $
    fastVScroll (vstack [
      h2 "Symbols" `styleBasic` [padding u],
      vgrid (map (hgrid . map symbolItem) symbolChunks),

      h2 "Rules" `styleBasic` [padding u],

      subsection "Propositional Logic",
      vstack $ map ruleItem visualRuleNames0,

      subsection "First Order Logic",
      vstack $ map ruleItem visualRuleNames1
    ]) `styleBasic` [borderL 1 dividerColor, rangeWidth 150 500]
    where
      hasChanged _wenv old new =
        old ^. persistentState . fileExplorerOpen /= new ^. persistentState . fileExplorerOpen ||
        old ^. persistentState . rulesSidebarOpen /= new ^. persistentState . rulesSidebarOpen

      subsection t = box (bold (span t)) `styleBasic` [padding u]

      ruleItem (key, label) = box_ [
          onClick clickEvent,
          onClickEmpty clickEvent
        ] (symbolSpan label)
        `styleBasic` [cursorHand, padding u, borderT 1 dividerColor]
        `styleHover` [bgColor hoverColor]
        `styleActive` [bgColor selectedColor]
        where clickEvent = OpenRuleGuide (Just key)

      symbolItem s =
        fastTooltip ("Insert character: " <> s) $
          box_ [onClick (SimulateTextInput s), onClickEmpty (SimulateTextInput s)] (symbolSpan s)
            `styleBasic` [cursorHand, padding u, borderT 1 dividerColor]
            `styleHover` [bgColor hoverColor]
            `styleActive` [bgColor selectedColor]

      symbolChunks = chunksOf 3 symbolsList

  contextMenuUI = popupV_ (model ^. contextMenu . ctxOpen) (\s -> if s then NoEvent else CloseContextMenu) [popupOpenAtCursor]
    (boxShadow $ vstack (map dropdownButton actions)
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

  confirmActionUI = popupV_ (isJust cad) (const NoEvent) [popupAlignToWindow, popupDisableClose, alignCenter, alignMiddle]
    (boxShadow $ vstack_ [childSpacing] [
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

  fileSearcherUI = popup_ (fileSearcher . fsOpen) [popupAlignToWindow, alignCenter, alignMiddle] $
    boxShadow $ vstack_ [childSpacing] [
      keystroke [
        ("Enter", CloseFileSearcher),
        ("Enter", openSelectedEvent),
        ("Up", ChangeFileSearcherIndex (-1) (key (-1))),
        ("Down", ChangeFileSearcherIndex 1 (key 1))
      ] $
        textField_ (fileSearcher . fsInput) [placeholder "Search for files by name or extension", onChange (const ResetFileSearcherIndex :: Text -> AppEvent)]
          `styleBasic` [paddingH u, paddingV (0.75*u)]
          `nodeKey` "fileSearcher.input",
      widgetIf (not noResult) (fastVScroll_ [scrollFollowFocus] resultItems `nodeKey` "fileSearcher.resultScroll"),
      widgetIf noResult (emptyItem "No files found")
    ]
      `styleBasic` [bgColor popupBackground, border 1 dividerColor, radius 4, padding 6, width 600, height 450]
    where
      key n = WidgetKey $ "fileSearcher.item." <> showt ((selected + n) `mod` length matchingFiles)

      openSelectedEvent = fromMaybe NoEvent $ maybeIndex matchingFiles selected >>= Just . OpenFile
      noResult = null matchingFiles
      matchingFiles
        | input == "" = take 100 allFiles
        | otherwise = take 100 $ filter (\f -> input `isInfixOf` pack f) allFiles
      input = model ^. fileSearcher . fsInput
      selected = model ^. fileSearcher . fsSelected `mod` length matchingFiles
      allFiles = model ^. fileSearcher . fsAllFiles

      resultItems = vstack (zipWith listItem matchingFiles [0..])

      listItem filePath idx = box_ [onClick (OpenFile filePath), onClick CloseFileSearcher, alignLeft] (span (pack filePath))
        `styleBasic` [paddingH u, paddingV (0.75 * u), radius 4, cursorHand, styleIf isSelected (bgColor selectedColor)]
        `styleHover` [bgColor hoverColor]
        `nodeKey` ("fileSearcher.item." <> showt idx)
        where isSelected = idx == selected

      emptyItem text = box_ [alignLeft] (span (pack text))
        `styleBasic` [paddingH u, paddingV (0.75 * u), radius 4, cursorHand]
        `styleHover` [bgColor hoverColor]

  exportOptionsUI = popup_ (exportOptionsPopup . eoOpen) [popupAlignToWindow, alignCenter, alignMiddle] $
    boxShadow $ vstack_ [childSpacing] [
      h2 "Export proof",
      paragraph "Proofs can be exported as either .tex source or rendered to pdf",
      spacer,
      bold $ span "Title",
      textField_ (exportOptionsPopup . eoTitle) [placeholder "No title"],
      filler,
      latexCompilerLabel (model ^. exportOptionsPopup . eoLatexCompiler),
      statusLabel (model ^. exportOptionsPopup . eoStatus),
      hstack_ [childSpacing] [
        button "Cancel" (SetExportOpen False),
        mainButton "Export PDF" ExportToPDF,
        mainButton "Export LaTeX" ExportToLaTeX
      ]
    ]
      `styleBasic` [bgColor popupBackground, border 1 dividerColor, radius 4, padding 16, width 600, height 450]
    where
      latexCompilerLabel (Right _) = span ""
      latexCompilerLabel (Left ex) = paragraph ("Export PDF not available\n" <> pack ex) `styleBasic` [textColor red]

      statusLabel ExportIdle = span ""
      statusLabel ExportWaiting = span "Generating file..."
      statusLabel ExportSuccess = span "Success" `styleBasic` [textColor green]
      statusLabel (ExportError t) = paragraph ("Export error: " <> t) `styleBasic` [textColor red]

  ruleGuideUI = popupV_ (isJust rg) (\s -> if s then NoEvent else OpenRuleGuide Nothing) [popupAlignToWindow, alignCenter, alignMiddle]
    (boxShadow $ vstack_ [childSpacing] [
      h1 name,
      paragraph description,
      widgetMaybe widget id
    ] `styleBasic` [bgColor popupBackground, border 1 dividerColor, padding 20, radius 4, width 600, height 450])
    where
      name = fromMaybe "" (info >>= Just . _riName)
      description = fromMaybe "" (info >>= Just . _riDescription)
      widget = (rg >>= flip Data.Map.lookup ruleUseWidgetList) >>= \w -> Just $ vstack_ [childSpacing] w
      info = rg >>= flip Data.Map.lookup ruleDescriptions
      rg = model ^. ruleGuidePopup

  ruleUseWidgetList = Data.Map.fromList [
      ("assume",  [box (box (aR "1. p" "assume") `styleBasic` [padding 10, border 2 proofBoxColor])]),
      ("copy",    [s "1. p", aR "2. p" "copy 1"]),
      ("AndI",    [s "1. p", s "2. q", aR "3. p ∧ q" "∧I (1,2)"]),
      ("AndEL",   [s "1. p ∧ q", aR "2. p" "∧EL 1"]),
      ("AndER",   [s "1. p ∧ q", aR "2. q" "∧EL 1"]),
      ("OrIL",    [s "1. p", aR "2. q ∨ p" "∨IL 1"]),
      ("OrIR",    [s "1. p", aR "2. p ∨ q" "∨IL 1"]),
      ("OrE",     [s "1. p ∨ q", sP "2. " "p" borderB, dot, sP "h. " "r" borderT, sP "i. " "q" borderB, dot, sP "j. " "r" borderT, aR "k. r" "∨E (1,2-h,i-j)"]),
      ("ImplI",   [sP "1. " "p" borderB, dot, sP "i. " "q" borderT, aR "j. p → q" "→I (1-i)"]),
      ("ImplE",   [s "1. p", s "2. p → q", aR "3. q" "→E (1,2)"]),
      ("NotI",    [sP "1. " "p" borderB, dot, sP "i. " "⊥" borderT, aR "j. ¬p" "¬I (1-i)"]),
      ("NotE",    [s "1. p", s "2. ¬p", aR "3. ⊥" "¬E (1,2)"]),
      ("BotE",    [s "1. ⊥", aR "2. p" "⊥E 1"]),
      ("NotNotI", [s "1. p", aR "2. ¬¬p" "¬¬I 1"]),
      ("NotNotE", [s "1. ¬¬p", aR "2. p" "¬¬E 1"]),
      ("MT",      [s "1. p → q", s "2. ¬q", aR "3. ¬p" "MT (1,2)"]),
      ("PBC",     [sP "1. " "¬p" borderB, dot, sP "i. " "⊥" borderT, aR "j. p" "PBC (1-i)"]),
      ("LEM",     [aR "1. p ∨ ¬p" "LEM"]),

      ("fresh", [box (box (aR "1. x₀" "fresh") `styleBasic` [padding 10, border 2 proofBoxColor])]),
      ("EqI", [aR "1. t = t" "=I"]),
      ("EqE", [s "1. t₁ = t₂", s "P[t₁/x]", aR "P[t₂/x]" "=E (1,2 w. ɸ≡P(x))"]),
      ("AllE", [s "1. ∀x.P(x)", aR "2. P[t/x]" "∀E 1 w. t"]),
      ("AllI", [sP "1. " "x₀" borderB, dot, sP "i. " "P[x₀/x]" borderT, aR "j. ∀x.P(x)" "∀I (1-i)"]),
      ("SomeE", [s "1. ∃x.P(x)", sP "2. " "x₀" borderB, sPM "3. " "P[x₀/x]", dot, sP "i. " "q" borderT, aR "j. q" "∃E (1,2-i)"]),
      ("SomeI", [s "1. P[t/x]", aR "2. ∃x.P(x)" "∃I 1"])
    ]
    where
      s f = hstack [symbolSpan f]
      aR o r = hstack [symbolSpan o, filler, symbolSpan r]
      sP i f b = hstack [s i, box (box (hstack [s f, filler]) `styleBasic` [padding 10, border 2 proofBoxColor, b 2 popupBackground])]
      sPM i f = hstack [s i, box (box (hstack [s f, filler]) `styleBasic` [padding 10, border 2 proofBoxColor, borderT 2 popupBackground, borderB 2 popupBackground])]
      dot = s "  ..."

  userDefinedRuleUI = popup_ udrPopup [popupAlignToWindow, alignCenter, alignMiddle] $
    widgetMaybe doc ui
    where
      doc = getCurrentFEDocument model
      ui doc = boxShadow $
        vstack_ [childSpacing] [
          h2 "User defined rules for this proof",
          maybeHeader,
          noUDRNote,
          vstack_ [childSpacing] (zipWith udrItem rules [0..]),
          button "+ Rule" AddUDR
        ] `styleBasic` [bgColor popupBackground, border 1 dividerColor, padding 20, radius 4, width 800, height 550]
        where
          maybeHeader = widgetIf (not $ null rules) udrHeader
          noUDRNote = widgetIf (null rules) (span "No rules have been defined yet.")
          rules = fromMaybe [] (_fedUserDefinedRules doc)

      udrHeader = hstack_ [childSpacing] [
          span "Name"
            `styleBasic` [width 125],
          span "Proof location"
            `styleBasic` [width 300],

          hgrid [
            span "Inputs",
            span "Output"
          ]
            `styleBasic` [expandWidth 1],

          span ""
            `styleBasic` [width 25]
        ]
          `styleBasic` [paddingT 10]

      udrItem rule idx = hstack_ [childSpacing] [
          textFieldV_ (_udrName rule) (\t -> EditUDR idx (editNameInUDR t rule)) [placeholder "Rule name"]
            `styleBasic` [width 125],
          textFieldV_ (pack $ _udrPath rule) (EditUDRPath idx . unpack) [placeholder "Relative or absolute path"]
            `styleBasic` [width 300],

          hgrid [
            input,
            output
          ]
            `styleBasic` [expandWidth 1],

          trashButton (RemoveUDR idx),
          fastTooltip "Refresh" $
            iconButton remixRestartLine (EditUDRPath idx (_udrPath rule))
        ]
        where
          input = maybe invalid (span . intercalate ",") (_udrInput rule)
          output = maybe invalid span (_udrOutput rule)
          invalid = span "Invalid" `styleBasic` [textColor red]

-- | Converts a list of font styles for a given font to a readable name
fontListToText :: [String] -> Text
fontListToText fontList | head fontList == "Regular" = "Default"
                        | head fontList == "Roboto_Regular" = "Roboto"
                        | head fontList == "Comic_Sans_Thin" = "Comic Sans"
                        | head fontList == "Dyslexic" = "Dyslexic"
                        | otherwise = "forgor_to_label"