{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}

module Frontend.HandleEvent (
  handleEvent
) where

import Frontend.Types
import Frontend.Helper.General
import Frontend.Helper.ProofHelper
import Frontend.Communication (startCommunication)
import Frontend.Parse
import Frontend.Preferences
import Frontend.Export (convertToLatex, compileLatexToPDF, latexCompilerAvailable)
import Frontend.History ( applyRedo, applyUndo, canUndoRedo )
import Shared.Messages
import Backend.Types (Result(..))
import Backend.TypeChecker (checkFE)

import Monomer
import NativeFileDialog ( openFolderDialog, openSaveDialog, openDialog )
import Control.Lens hiding ((<.>))
import Control.Exception (try, SomeException)
import Control.Concurrent ( newChan, threadDelay )
import Control.Concurrent.STM.TChan
import Control.Monad (forever, when, filterM, foldM)
import Control.Monad.Extra (partitionM)
import Data.Maybe (fromMaybe)
import Data.Text (unpack, pack, Text)
import Data.List (sort)
import TextShow ( TextShow(showt) )
import System.Directory ( removeFile, createDirectoryIfMissing, listDirectory, doesFileExist, doesDirectoryExist, makeAbsolute )
import System.FilePath (takeExtension, (</>), (<.>), isRelative, takeDirectory, equalFilePath)
import System.IO (readFile')

import qualified SDL
import Control.Concurrent.STM (atomically)
import Monomer.Helper (collectJustM)

import qualified SDL.Raw
import Foreign (alloca, poke)
import Foreign.C (castCharToCChar)
import qualified Data.ByteString.Char8 as C8
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Encoding as TE

import Graphics.UI.TinyFileDialogs (openFileDialog, saveFileDialog, selectFolderDialog)

handleEvent
  :: Text
  -> AppEnv
  -> WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent os env wenv node model evt = case evt of
  NoEvent -> []

  AppRunProducer prod -> [ Producer prod ]

  Undo -> if canUndoRedo model
    then applyOnCurrentFile model applyUndo
    else []

  Redo -> if canUndoRedo model
    then applyOnCurrentFile model applyRedo
    else []

  AppInit -> [
      Producer (startDebouncer initialWait env),
      Producer $ directoryFilesProducer (model ^. persistentState . workingDir),
      Task $ do
        frontendChan <- newChan
        backendChan <- newChan
        answer <- startCommunication frontendChan backendChan
        return $ BackendResponse answer
    ] ++ firstTimeEvent
    where
      initialWait = not $ model ^. preferences . autoCheckProofTracker . acpEnabled
      firstTimeEvent = if model ^. persistentState . firstTime
        then [
          Model $ model & persistentState . firstTime .~ False,
          Event OpenWelcome
        ]
        else []

  AppBeforeExit -> [
      cancelExitApplication,
      Producer (savePrefAndState model ExitApp)
    ]

  ExitApp -> [ exitApplication ]

  AppResize m -> [ Model $ model & persistentState . windowMode .~ m ]

  CopyToClipboard t -> [ Producer (\_ -> SDL.setClipboardText t) ]

  SimulateTextInput t -> [ Producer (const (simulateTextInput t)) ]

  OpenUDR -> case model ^. persistentState . currentFile of
    Nothing -> []
    Just filePath -> case currentFile of
      Just ProofFile {} -> open
      Just TemporaryProofFile {} -> open
      _ -> []
      where
        currentFile = getProofFileByPath (model ^. persistentState . tmpLoadedFiles) filePath
        open = [ Model $ model & udrPopup .~ True ]

  AddUDR -> applyOnCurrentFEDocument model (addUDRToDocument newUDR)
    where newUDR = FEUserDefinedRule "" "" Nothing Nothing

  RemoveUDR idx -> applyOnCurrentFEDocument model (removeUDRFromDocument idx)

  EditUDRPath idx newPath -> Producer (\sendMsg -> do
      let parentPath = fromMaybe "" (model ^. persistentState . currentFile)
      let fullPath = if isRelative newPath
          then takeDirectory parentPath </> newPath
          else newPath

      pContent <- try (readFile' fullPath) :: IO (Either SomeException String)
      case pContent of
        Left e -> do
          print e
          sendMsg invalidEvent
        Right pContent -> do
          let pContentText = pack pContent
          if takeExtension fullPath `elem` map ("." <>) feFileExts then do
            let doc = parseProofFromJSON pContentText
            case doc of
              Nothing -> do
                putStrLn "No document"
                sendMsg invalidEvent
              Just doc -> do
                case checkFE doc of
                  Err {} -> sendMsg invalidEvent
                  Ok {} -> do
                    let input = Just $ (_premises . _sequent) doc
                    let output = Just $ (_conclusion . _sequent) doc
                    sendMsg (EditUDRIO idx input output)
          else do
            putStrLn "Not a proof"
            sendMsg invalidEvent
    ) : applyOnCurrentFEDocument model (editUDRInDocument idx (editPathInUDR newPath))
    where invalidEvent = EditUDRIO idx Nothing Nothing

  EditUDRIO idx input output -> applyOnCurrentFEDocument model f
    where f = editUDRInDocument idx (editIOInUDR input output)

  EditUDR idx newUDR -> applyOnCurrentFEDocument model (editUDRInDocument idx (const newUDR))

  OpenRuleGuide i -> [ Model $ model & ruleGuidePopup .~ i ]

  OpenConfirmAction a -> [ Model $ model & confirmActionPopup %~ openPopup ]
    where
      openPopup Nothing = Just a
      openPopup old@(Just _) = old

  CloseConfirmAction a -> [
      Model $ model & confirmActionPopup .~ Nothing,
      Event a
    ]

  SetOpenMenuBarItem s -> [ Model $ model & openMenuBarItem .~ s ]

  OpenContextMenu actions -> [
      Model $ model
        & contextMenu . ctxActions .~ actions
        & contextMenu . ctxOpen .~ True
    ]

  CloseContextMenu -> [ Model $ model & contextMenu . ctxOpen .~ False ]

  DeleteFilePath p -> [ Producer (\sendMsg -> do
      result <- try (removeFile p) :: IO (Either SomeException ())
      case result of
        Left e -> print e
        Right _ -> sendMsg RefreshExplorer
    ) ]

  OpenInExplorer p -> [ Producer (\_ -> openInExplorer wenv p) ]

  FocusOnKey key -> [ SetFocusOnKey key ]

  NextFocus n -> replicate n (MoveFocusFromKey Nothing FocusFwd)

  OpenFileSearcher
    | model ^. fileSearcher . fsOpen -> []
    | otherwise -> case model ^. persistentState . workingDir of
        Nothing -> []
        Just wd -> [
            Event $ SetAllFilesInFileSearcher [],
            Producer (\sendMsg -> do
              allFiles <- traverseDir (const True) (\fs f -> pure (f : fs)) [] wd
              sendMsg $ SetAllFilesInFileSearcher $ sort allFiles
            ),
            Model $ model
              & fileSearcher . fsOpen .~ True
              & fileSearcher . fsInput .~ ""
              & fileSearcher . fsSelected .~ 0,
            SetFocusOnKey $ WidgetKey "fileSearcher.item.0",
            SetFocusOnKey $ WidgetKey "fileSearcher.input"
          ]

  CloseFileSearcher -> [ Model $ model & fileSearcher . fsOpen .~ False ]

  ChangeFileSearcherIndex n nodeKey -> [
      Model $ model & fileSearcher . fsSelected %~ (+n),
      SetFocusOnKey nodeKey,
      SetFocusOnKey $ WidgetKey "fileSearcher.input"
    ]

  ResetFileSearcherIndex -> [ Model $ model & fileSearcher . fsSelected .~ 0 ]

  SetAllFilesInFileSearcher filePaths -> [ Model $ model & fileSearcher . fsAllFiles .~ filePaths ]

  AddPremise idx -> events
    where
      events = editEvent ++ focusEvent
      focusEvent = [ SetFocusOnKey (WidgetKey $ "premise.input." <> showt (idx + 1)) ]
      editEvent = applyOnCurrentProofAndRecordSequentHistory model (addPremiseToProof (idx + 1) "")

  RemovePremise idx -> events
    where
      events = editEvent ++ getEventUsingCurrentSequent model getFocusEvent
      getFocusEvent seq
        | length (_premises seq) == 1 = [ SetFocusOnKey "addPremiseButton" ]
        | length (_premises seq) == idx + 1 = [ SetFocusOnKey (WidgetKey $ "premise.input." <> showt (idx - 1)) ]
        | otherwise = [ SetFocusOnKey (WidgetKey $ "premise.input." <> showt idx) ]
      editEvent = applyOnCurrentProofAndRecordSequentHistory model (removePremiseFromProof idx)

  EditPremise idx newText -> applyOnCurrentProofAndRecordHistory model f
    where f seq = (editPremisesInProof idx newText seq, HEditPremise idx (_premises seq !! idx) newText)

  EditConclusion newText -> applyOnCurrentProofAndRecordHistory model f
    where f seq = (editConclusionInProof newText seq, HEditConclusion (_conclusion seq) newText)

  SwitchLineToSubProof path widgetKey -> applyOnCurrentProofAndRecordHistory model f ++ [SetFocusOnKey widgetKey]
    where
      f seq = (newSeq, HEditStep path (evalPath path seq) (evalPath path newSeq))
        where newSeq = switch seq
      switch = replaceInProof path (\oldLine -> SubProof [oldLine])

  SwitchSubProofToLine path widgetKey -> applyOnCurrentProofAndRecordHistory model f ++ [SetFocusOnKey widgetKey]
    where
      f seq = (newSeq, HEditStep path (evalPath path seq) (evalPath path newSeq))
        where newSeq = switch seq
      switch p = if not $ isSingleton $ evalPath path p then p else replaceInProof path (\oldLine -> case oldLine of
          SubProof p -> head p
          _ -> error ""
        ) p
      isSingleton (SubProof p) = length p == 1
      isSingleton _ = False

  InsertLineAfter updateRef path -> applyOnCurrentProofAndRecordSequentHistory model f ++ focusAction
    where
      f = insertAfterAndMaybeUpdateRefs path insertThis updateRef
      -- f seq = (insertAfterAndMaybeUpdateRefs path insertThis updateRef seq, HInsertStep updateRef nextPath insertThis)
      insertThis = Line "" "" 0 []
      focusAction = fromMaybe [] maybeFocusAction
      maybeFocusAction = (getCurrentSequent model >>= \f -> nextPath >>= Just . pathToLineNumber f) >>= getFocusAction
      getFocusAction l = Just [ SetFocusOnKey (WidgetKey $ showt l <> ".statement") ]
      nextPath = if null path
        then Nothing
        else Just $ init path ++ [last path + 1]

  InsertLineBefore updateRef path -> applyOnCurrentProofAndRecordSequentHistory model f ++ focusAction
    where
      f = insertBeforeAndMaybeUpdateRefs path insertThis updateRef
      -- f seq = (insertBeforeAndMaybeUpdateRefs path insertThis updateRef seq, HInsertStep updateRef nextPath insertThis)
      insertThis = Line "" "" 0 []
      focusAction = fromMaybe [] maybeFocusAction
      maybeFocusAction = (getCurrentSequent model >>= \f -> nextPath >>= Just . pathToLineNumber f) >>= getFocusAction
      getFocusAction l = Just [ SetFocusOnKey (WidgetKey $ showt l <> ".statement") ]
      nextPath = if null path
        then Nothing
        else Just $ init path ++ [last path + 0]

  InsertSubProofAfter updateRef path -> applyOnCurrentProofAndRecordSequentHistory model f
    where
      f = insertAfterAndMaybeUpdateRefs path insertThis updateRef
      -- f seq = (insertAfterAndMaybeUpdateRefs path insertThis updateRef seq, HInsertStep updateRef nextPath insertThis)
      -- nextPath = init path ++ [last path + 1]
      insertThis = SubProof [Line "" "" 0 []]

  InsertSubProofBefore updateRef path -> applyOnCurrentProofAndRecordSequentHistory model f
    where
      f = insertBeforeAndMaybeUpdateRefs path insertThis updateRef
      -- f seq = (insertBeforeAndMaybeUpdateRefs path insertThis updateRef seq, HInsertStep updateRef path insertThis)
      insertThis = SubProof [Line "" "" 0 []]

  AddLine -> applyOnCurrentProofAndRecordSequentHistory model f
    where
      f seq = insertAfterProof (pathToLastLine seq) insertThis seq
      -- f seq = (insertAfterProof path insertThis seq, HInsertStep False (nextSibling path) insertThis)
      --   where path = pathToLastLine seq
      insertThis = Line "" "" 0 []

  AddSubProof -> applyOnCurrentProofAndRecordSequentHistory model f
    where
      f seq = insertAfterProof (pathToLastLine seq) insertThis seq
      -- f seq = (insertAfterProof path insertThis seq, HInsertStep False (nextSibling path) insertThis)
      --   where path = pathToLastLine seq
      insertThis = SubProof [Line "" "" 0 []]

  RemoveLine updateRef path -> applyOnCurrentProofAndRecordSequentHistory model removeLine ++ focusAction
    where
      removeLine = removeFromProof path True . offsetFunc
      offsetFunc seq = if updateRef then offsetAllRefs path (-1) lineNumber True seq else seq
        where lineNumber = pathToLineNumberOffsetPremises seq path

      focusAction = fromMaybe [] (getCurrentSequent model >>= Just . getFocusAction)
      getFocusAction sequent = [ SetFocusOnKey (WidgetKey $ showt l <> ".statement") ]
        where l = pathToLineNumber sequent path - 1

  -- RemoveLine _updateRef path -> applyOnCurrentFile model removeLine
  --   where
  --     removeLine file =
  --       file
  --         & parsedSequent %~ maybeF (removeFromProof path True)
  --         & history . hIndex %~ (+1)
  --         & history . hState %~ forkHistory (_hIndex (_history file))
  --       where
  --         forkHistory index state = fromMaybe state $ _parsedSequent file >>= Just . f
  --           where
  --             f seq = take (index + 1) state ++ [HMultiple (reverse extraEvents ++ [HRemoveStep path rs])]
  --               where
  --                 rs = evalPath path seq
  --                 extraEvents = snd $ removeInvalidFromProofWithHistory (removeFromProof path False seq)

  --     maybeF f (Just s) = Just (f s)
  --     maybeF _ Nothing = Nothing

  -- RemoveLine updateRef path -> applyOnCurrentProofAndRecordHistory model f ++ focusAction
  --   where
  --     f seq = (removeLine seq, historyEvent)
  --       where
  --         historyEvent = HMultiple (reverse cleanupEvents ++ [removeEvent])
  --         removeEvent = HRemoveStep updateRef path (evalPath path seq)
  --         cleanupEvents = snd $ removeInvalidFromProofWithHistory (removeFromProof path False seq)

  --     removeLine = removeFromProof path True . offsetFunc
  --     offsetFunc seq = if updateRef then offsetAllRefs (-1) lineNumber True seq else seq
  --       where lineNumber = pathToLineNumberOffsetPremises seq path

  --     focusAction = fromMaybe [] (getCurrentSequent model >>= Just . getFocusAction)
  --     getFocusAction sequent = [ SetFocusOnKey (WidgetKey $ showt l <> ".statement") ]
  --       where l = pathToLineNumber sequent path - 1

  -- RemoveLine _ path -> [ Producer (\sendMsg -> do
  --     case model ^. persistentState . currentFile of
  --       Nothing -> return ()
  --       Just filePath -> do
  --         case getProofFileIndexByPath (model ^. persistentState . tmpLoadedFiles) filePath of
  --           Nothing -> return ()
  --           Just idx -> do
  --             let file = model ^. persistentState . tmpLoadedFiles . singular (ix idx)
  --             case _parsedSequent file of
  --               Nothing -> return ()
  --               Just seq -> do
  --                 let newSeqAndEvents = removeInvalidFromProofWithHistory (removeFromProof path False seq)
  --                 print $ snd newSeqAndEvents
  --                 return ()
  --   )]

  EditFormula path newText -> editLineAndRecordHistory model path (editFormulaInProof model path newText)

  EditRuleName path newText -> editLineAndRecordHistory model path (editRuleNameInProof path newText)

  EditRuleArgument path idx newText -> editLineAndRecordHistory model path (editRuleArgumentInProof path idx newText)

  MovePathToPath target dest
    | pathIsParentOf target (nextSibling dest) -> []
    | otherwise -> applyOnCurrentProofAndRecordSequentHistory model f
      where f = moveInProof target (nextSibling dest) True

  -- MovePathToPath target dest
  --   | pathIsParentOf target (nextSibling dest) -> []
  --   | otherwise -> applyOnCurrentProofAndRecordHistory model f
  --     where
  --       f seq = (newSeq, historyEvent)
  --         where
  --           newSeq = moveInProof target (nextSibling dest) True seq
  --           historyEvent = HMoveStep (_steps seq) (_steps newSeq)
  --           -- historyEvent = HMultiple (reverse cleanupEvents ++ [moveEvent])
  --           -- moveEvent = HMoveStep target (nextSibling dest)
  --           -- cleanupEvents = snd $ removeInvalidFromProofWithHistory (moveInProof target (nextSibling dest) False seq)

  CreateEmptyProof -> [
      Producer (\sendMsg -> do
        randomFileName <- getTmpFileName
        tmpBasePath <- getTmpBasePath
        createDirectoryIfMissing False tmpBasePath
        let randomPath = (tmpBasePath </> randomFileName) <.> "tmp"

        result <- try (writeFile randomPath "") :: IO (Either SomeException ())
        case result of
          Left e -> print e
          Right _ -> do
            let emptySeq = FESequent [] "" [Line "" "" 0 []]
            let emptyDoc = Just $ FEDocument {
              _fedUserDefinedRules = Nothing,
              _sequent = emptySeq
            }
            let emptyHistory = History {
              _hState = [],
              _hIndex = -1
            }
            let file = TemporaryProofFile randomPath (Just "New proof") emptyDoc False emptyHistory
            sendMsg (OpenFileSuccess file)
            sendMsg (FocusOnKey "addPremiseButton")
      )
    ]

  ToggleRulesSidebar -> [ Model $ model & persistentState . rulesSidebarOpen %~ toggle]
    where toggle = not

  ToggleFileExplorer -> [ Model $ model & persistentState . fileExplorerOpen %~ toggle]
    where toggle = not

  RefreshExplorer -> [
      Model $ model & filesInDirectory ?~ LoadedFiles [] [],
      Producer $ directoryFilesProducer (model ^. persistentState . workingDir)
    ]

  SetFilesInDirectory fs -> [ Model $ model & filesInDirectory .~ fs ]

  OpenPreferences -> [ Producer (\sendMsg -> do
      preferencePath <- getPreferencePath
      sendMsg (OpenFile_ preferencePath "" (Just "Preferences"))
    )]

  OpenGuide -> [ Producer (\sendMsg -> do
      basePath <- getAssetBasePath
      let docsPath = basePath </> "assets" </> "docs"
      sendMsg $ OpenFile_ "user_guide_en.md" docsPath (Just "Guide")
    ) ]

  OpenWelcome -> [ Producer (\sendMsg -> do
      basePath <- getAssetBasePath
      let docsPath = basePath </> "assets" </> "docs"
      sendMsg $ OpenFile_ "welcome.md" docsPath (Just "Welcome")
    ) ]

  OpenAbout -> [ Producer (\sendMsg -> do
      basePath <- getAssetBasePath
      let docsPath = basePath </> "assets" </> "docs"
      sendMsg $ OpenFile_ "about.md" docsPath (Just "About")
    ) ]

  OpenFileFromFileSystem -> [ Task openDiag ]
    where
      openDiag = do
        path <- osOpenFileDialog
          os
          "Open proof"
          ""
          feFileExts
          feFileExtName
          False

        case path of
          Nothing -> return NoEvent
          Just path -> return $ OpenFile_ path "" Nothing

  OpenFileExample -> [ Task openDiag ]
    where
      openDiag = do
        basePath <- getAssetBasePath
        let defaultPath = basePath </> "assets/examples/"
        absDefPath <- makeAbsolute defaultPath

        path <- osOpenFileDialog
          os
          "Open example"
          absDefPath
          feFileExts
          feFileExtName
          False

        case path of
          Nothing -> return NoEvent
          Just path -> return $ OpenFile_ path "" Nothing

  OpenFile filePath -> handleEvent os env wenv node model (OpenFile_ filePath wd Nothing)
    where wd = fromMaybe "" (model ^. persistentState . workingDir)

  OpenFile_ filePath folderPath tabDisp -> [
      Producer (\sendMsg -> do
        preferencePath <- getPreferencePath
        let fullPath = folderPath </> filePath
        pContent <- try (readFile' fullPath) :: IO (Either SomeException String)
        case pContent of
          Left e -> print e
          Right pContent -> do
            let pContentText = pack pContent
            let pIsEdited = False
            let pHistory = History {
              _hState = [],
              _hIndex = -1
            }

            if takeExtension fullPath == ".md" then
              sendMsg (OpenFileSuccess $ MarkdownFile fullPath tabDisp pContentText)
            else if fullPath == preferencePath && folderPath == "" then
              sendMsg (OpenFileSuccess $ PreferenceFile fullPath tabDisp pIsEdited)
            else if takeExtension fullPath `elem` map ("." <>) feFileExts then
              do
                let doc = parseProofFromJSON pContentText
                sendMsg (OpenFileSuccess $ ProofFile fullPath tabDisp pContentText doc pIsEdited pHistory)
            else
              sendMsg (OpenFileSuccess $ OtherFile fullPath tabDisp pContentText)
      )
    ]

  OpenFileSuccess file -> [ Producer (\sendMsg -> do
      sendMsg $ OpenFileSuccess_ file
      when (model ^. preferences . autoCheckProofTracker . acpEnabled) $
        sendMsg CheckCurrentProof
    ) ]

  OpenFileSuccess_ file -> Model newModel : handleEvent os env wenv node newModel (SetCurrentFile filePath)
    where
      newModel = model
        & persistentState . openFiles %~ doOpenFile
        & persistentState . tmpLoadedFiles %~ createNew file

      doOpenFile currentlyOpenFiles = currentlyOpenFiles ++ [filePath | all (\f -> not (equalFilePath filePath f)) (model ^. persistentState . openFiles)]
      createNew newFile oldFiles = if _path newFile `elem` (model ^. persistentState . openFiles) then
          oldFiles else
          filter (\f -> _path newFile /= _path f) oldFiles ++ [newFile]
      filePath = _path file

  CloseCurrentFile -> case model ^. persistentState . currentFile of
    Nothing -> []
    Just filePath -> handleEvent os env wenv node model (CloseFile filePath)

  CloseFile filePath -> case file of
    Nothing -> []
    Just file -> (if isFileEdited (Just file)
      then handleEvent os env wenv node model (OpenConfirmAction (ConfirmActionData {
        _cadTitle = "Close without saving?",
        _cadBody = "Are you sure you want to close\n" <> displayedPath <> "\nwithout saving? All changes will be lost!",
        _cadAction = CloseFileSuccess filePath
      }))
      else handleEvent os env wenv node model (CloseFileSuccess filePath))
      where
        displayedPath
          | isTmpFile filePath = "Unsaved proof"
          | otherwise = pack filePath
    where file = getProofFileByPath (model ^. persistentState . tmpLoadedFiles) filePath

  CloseFileSuccess filePath -> Model finalModel : deleteTmp
    where
      deleteTmp = [ Producer (\_ -> do
          result <- try (removeFile filePath) :: IO (Either SomeException ())
          case result of
            Left e -> print e
            Right _ -> return ()
        ) | isTmpFile filePath]
      finalModel = modelWithClosedFile
        & persistentState . currentFile .~ (if cf == Just filePath then maybeHead (modelWithClosedFile ^. persistentState . openFiles) else cf)
      modelWithClosedFile = model
        & persistentState . openFiles %~ filter (filePath/=)
        & proofStatus .~ Nothing
      cf = model ^. persistentState . currentFile

  SaveCurrentFile -> case model ^. persistentState . currentFile of
    Nothing -> []
    Just filePath -> case currentFile of
      Just file@ProofFile {} -> handleEvent os env wenv node model (SaveFile file)
      Just file@TemporaryProofFile {} -> handleEvent os env wenv node model (SaveFile file)
      Just file@PreferenceFile {} -> handleEvent os env wenv node model (SaveFile file)
      _ -> []
      where currentFile = getProofFileByPath (model ^. persistentState . tmpLoadedFiles) filePath

  SaveFile f -> case f of
    PreferenceFile {} -> handleEvent os env wenv node model SavePreferences
    f@ProofFile {} -> case _parsedDocument f of
      Nothing -> []
      Just doc -> [
          Producer (\sendMsg -> do
            let content = (unpack . parseProofToJSON) doc
                fileName = _path f

            result <- try (writeFile fileName content) :: IO (Either SomeException ())
            case result of
              Left e -> print e
              Right _ -> sendMsg (SaveFileSuccess f)
          )
        ]
    f@TemporaryProofFile {} -> case _parsedDocument f of
      Nothing -> []
      Just doc -> [
        Task $ do
          mNewPath <- osSaveFileDialog
            os
            "Save proof"
            ""
            feFileExts
            feFileExtName

          return $ AppRunProducer (\sendMsg -> do
            case mNewPath of
              Nothing -> return ()
              Just newPath -> do
                let fixedPath = if takeExtension newPath == ""
                    then newPath <.> head feFileExts
                    else newPath
                let content = (unpack . parseProofToJSON) doc
                result <- try (writeFile fixedPath content) :: IO (Either SomeException ())
                case result of
                  Left e -> print e
                  Right _ -> do
                    let tmpPath = _path f
                    sendMsg (SaveFileSuccess f)
                    sendMsg (CloseFileSuccess tmpPath)
                    sendMsg RefreshExplorer
                    sendMsg (OpenFile_ fixedPath "" Nothing)
            )
        ]
    f -> error $ "Cannot save file of type " ++ show f

  SaveFileSuccess f -> actions
    where
      actions = fromMaybe [] (fileIndex >>= Just . getActions)
      getActions fileIndex = [ Model $ model & persistentState . tmpLoadedFiles . singular (ix fileIndex) . isEdited .~ False ]
      fileIndex = getProofFileIndexByPath (model ^. persistentState . tmpLoadedFiles) (_path f)

  SetCurrentFile filePath -> [
      Model $ model
        & persistentState . currentFile ?~ filePath
        & proofStatus .~ Nothing,
        Producer (\sendMsg -> do
          when (model ^. preferences . autoCheckProofTracker . acpEnabled) (
            case file of
              Just file@ProofFile {} -> sendMsg $ CheckProof file
              _ -> return ()
            )
          )
    ] ++ [Event CheckCurrentProof | shouldCheck]
    where
      file = getProofFileByPath (model ^. persistentState . tmpLoadedFiles) filePath
      shouldCheck = model ^. preferences . autoCheckProofTracker . acpEnabled

  MoveTab fromIdx toIdx
    | fromIdx < toIdx -> [ Model $ model & persistentState . openFiles %~ removeIdx fromIdx . insertAt (model ^. persistentState . openFiles . element fromIdx) (toIdx + 1) ]
    | fromIdx > toIdx -> [ Model $ model & persistentState . openFiles %~ insertAt (model ^. persistentState . openFiles . element fromIdx) (toIdx + 1) . removeIdx fromIdx ]
    | otherwise -> []

  SwitchTheme -> [
      Model $ model & preferences . selectedTheme %~ switchTheme
    ]
    where
      switchTheme Light = Dark
      switchTheme Dark = Light

  UpdateFont s -> [Model $ model & preferences . normalFont .~ head s]
  ResetFontSize -> [Model $ model & preferences . fontSize .~ 16]
  ResetAppScale -> [Model $ model & preferences . appScale .~ 1]

  ReadPreferences -> [ Producer readAndApplyPreferences ]
  ReadPreferences_ prefs -> [ Model $ model & preferences .~ prefs ]

  SavePreferences -> [ Producer (savePreferences model NoEvent) ]

  CheckCurrentProof -> case model ^. persistentState . currentFile of
    Nothing -> []
    Just filePath -> case currentFile of
      Just file@ProofFile {} -> handleEvent os env wenv node model (CheckProof file)
      Just file@TemporaryProofFile {} -> handleEvent os env wenv node model (CheckProof file)
      _ -> []
      where currentFile = getProofFileByPath (model ^. persistentState . tmpLoadedFiles) filePath

  CheckProof file -> [
      -- Model $ model & proofStatus .~ Nothing,
      Producer (evaluateCurrentProof model file acpFlag wrngSensetivity)
    ]
    where acpFlag = model ^. preferences . autoCheckProofTracker . acpEnabled
          wrngSensetivity = model ^. preferences . warningMessageSeverity

  AutoCheckProof
    | model ^. preferences . autoCheckProofTracker . acpEnabled -> [ Task $ sendProofDidChange env ]
    | otherwise -> []

  BackendResponse (FEDocumentChecked result) -> [ Model $ model & proofStatus ?~ result ]
  BackendResponse (OtherBackendMessage message) -> [ Producer (\_ -> print $ "From backend: " ++ message) ]

  OpenSetWorkingDir -> [ Task openDiag ]
    where
      openDiag = do
        path <- osSelectFolderDialog
          os
          "Set working directory"
          ""
        case path of
          Nothing -> return NoEvent
          Just path -> return $ SetWorkingDir path

  SetWorkingDir path -> [
      Model $ model & persistentState . workingDir .~ newWd,
      Event RefreshExplorer
    ]
    where newWd = Just path

  SetExportOpen open -> [
      Model $ model
        & exportOptionsPopup . eoOpen .~ open
        & exportOptionsPopup . eoStatus .~ ExportIdle
        & exportOptionsPopup . eoLatexCompiler .~ Right (),
      Producer (\sendMsg -> do
          canCompile <- latexCompilerAvailable
          sendMsg (SetExportLatexCompiler canCompile)
        )
    ]

  SetExportLatexCompiler s -> [ Model $ model & exportOptionsPopup . eoLatexCompiler .~ s ]

  ExportToLaTeX -> case model ^. persistentState . currentFile of
    Nothing ->
      [Message (WidgetKey "ExportError") (pack "Please save your proof first")]
    Just filePath -> case getProofFileByPath (model ^. persistentState . tmpLoadedFiles) filePath of
      Just file@ProofFile{} -> exportToLatex os model file
      Just file@TemporaryProofFile{} -> exportToLatex os model file
      _ -> [Message (WidgetKey "ExportError") (pack "Only proof files can be exported")]

  ExportToPDF -> case model ^. persistentState . currentFile of
    Nothing ->
      [Message (WidgetKey "ExportError") (pack "Please save your proof first")]
    Just filePath -> case getProofFileByPath (model ^. persistentState . tmpLoadedFiles) filePath of
      Just file@ProofFile{} -> exportToPDF os model file
      Just file@TemporaryProofFile{} -> exportToPDF os model file
      _ -> [Message (WidgetKey "ExportError") (pack "Only proof files can be exported")]

  SetExportStatus status -> [ Model $ model & exportOptionsPopup . eoStatus .~ status]

  Print s -> [ Producer (\_ -> print s) ]
  -- f -> [ Producer (\_ -> print f) ]

{-|
Recursively gets all files in working directory and
sends back `SetFilesInDirectory` event with the files
-}
directoryFilesProducer :: Maybe FilePath -> (AppEvent -> IO ()) -> IO ()
directoryFilesProducer workingDir sendMsg = do
  case workingDir of
    Nothing -> sendMsg (SetFilesInDirectory Nothing)
    Just wd -> do
      result <- try (listDirectory wd) :: IO (Either SomeException [FilePath])
      case result of
        Left e -> do
          print e
          sendMsg (SetFilesInDirectory Nothing)
        Right allFileNames -> do
          let fullFilePaths = map appendTop allFileNames
          onlyFiles <- filterM doesFileExist fullFilePaths
          onlyDirs <- filterM doesDirectoryExist fullFilePaths
          let loadedFiles = LoadedFiles {
            _lFiles = onlyFiles,
            _lDirectories = onlyDirs
          }
          sendMsg (SetFilesInDirectory (Just loadedFiles))
      where
        appendTop :: FilePath -> FilePath
        appendTop = (wd </>)

{-|
"Debounce" changes in current proof and only check the proof
if no changes has been made for some time
-}
startDebouncer :: Bool -> AppEnv -> (AppEvent -> IO ()) -> IO ()
startDebouncer initialWait env sendMsg = do
  when initialWait $ do
    atomically $ readTChan channel
  forever $ do
    -- Get number of changes since last update
    inputs <- collectJustM . atomically $ tryReadTChan channel

    -- No new changes, check proof
    when (null inputs) $ do
      sendMsg CheckCurrentProof
      -- Wait here for next change
      atomically $ readTChan channel

    threadDelay _250ms
    where
      _250ms = 250 * 1000
      channel = env ^. envChannel

-- | Notify debouncer that changes have been made to proof
sendProofDidChange :: AppEnv -> IO AppEvent
sendProofDidChange env = do
  atomically $ writeTChan (env ^. envChannel) ()
  return NoEvent

-- | Send text event to SDL to fake keyboard input
simulateTextInput :: Text -> IO ()
simulateTextInput t = do
  let typ = SDL.Raw.SDL_TEXTINPUT
  let timestamp = 0
  let windowID = 0
  let text = map castCharToCChar . C8.unpack . TE.encodeUtf8 $ t <> "\x00"

  let rawEvent = SDL.Raw.TextInputEvent {
    SDL.Raw.eventType = typ,
    SDL.Raw.eventTimestamp = timestamp,
    SDL.Raw.textInputEventWindowID = windowID,
    SDL.Raw.textInputEventText = text
  }

  _ <- liftIO . alloca $ \eventPtr -> do
    poke eventPtr rawEvent
    SDL.Raw.pushEvent eventPtr

  return ()

-- From https://stackoverflow.com/a/51713361
-- | Recursivly search directory and filter
traverseDir :: (FilePath -> Bool) -> (b -> FilePath -> IO b) -> b -> FilePath -> IO b
traverseDir validDir transition =
    let go state dirPath =
            do names <- listDirectory dirPath
               let paths = map (dirPath </>) names
               (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
               state' <- foldM transition state filePaths -- process current dir
               foldM go state' (filter validDir dirPaths) -- process subdirs
     in go

exportToLatex :: Text -> AppModel -> File -> [EventResponse s AppEvent sp ep]
exportToLatex os model file = case _parsedDocument file of
  Nothing -> [Message (WidgetKey "ExportError") (pack "Cannot export invalid proof")]
  Just _ ->
    [ Task $ do
        -- Open a save dialog to let the user choose where to save the LaTeX file
        mSavePath <- osSaveFileDialog
          os
          "Export .tex file"
          ""
          ["tex"]
          "TeX files (.tex)"

        return $ AppRunProducer (\sendMsg -> do
          sendMsg (SetExportStatus ExportWaiting)
          case mSavePath of
            Nothing ->
              -- User canceled the dialog
              sendMsg (SetExportStatus (ExportError "Export canceled"))
            Just savePath -> do
              -- Generate proper LaTeX content using our export module
              let texPath = if takeExtension savePath == ".tex"
                            then savePath
                            else savePath <> ".tex"
                  title = model ^. exportOptionsPopup . eoTitle
                  latexContent = convertToLatex title model

              -- Write the full LaTeX content to the file
              writeResult <- try (writeFile texPath (unpack latexContent)) :: IO (Either SomeException ())
              case writeResult of
                Left ex -> print ex
                Right () -> do
                  putStrLn $ "Exported LaTeX file to: " ++ texPath
                  sendMsg (SetExportStatus ExportSuccess)
          )
    ]

exportToPDF :: Text -> AppModel -> File -> [EventResponse s AppEvent sp ep]
exportToPDF os model file = case _parsedDocument file of
  Nothing -> [Message (WidgetKey "ExportError") (pack "Cannot export invalid proof")]
  Just _ ->
    [ Task $ do
        -- Open a save dialog to let the user choose where to save the file
        mSavePath <- osSaveFileDialog
          os
          "Export PDF"
          ""
          ["pdf"]
          "PDF (.pdf)"
        return $ AppRunProducer (\sendMsg -> do
          sendMsg (SetExportStatus ExportWaiting)
          case mSavePath of
            Nothing -> do
              sendMsg (SetExportStatus (ExportError "Export canceled"))
            Just savePath -> do
              -- Compile the LaTeX to PDF (aux/log files will be in temp dir)
              let title = model ^. exportOptionsPopup . eoTitle
              result <- compileLatexToPDF savePath title model
              case result of
                Right _pdfPath -> do
                  sendMsg (SetExportStatus ExportSuccess)
                  -- sendMsg (ExportSuccess (pack $ "PDF created: " ++ pdfPath))
                Left err -> do
                  sendMsg (SetExportStatus (ExportError (pack $ "PDF compilation failed: " ++ err)))
                  -- sendMsg (ExportError (pack $ "PDF compilation failed: " ++ err))
          )
    ]

osOpenFileDialog :: Text -> Text -> FilePath -> [String] -> Text -> Bool -> IO (Maybe String)
osOpenFileDialog os title defaultPath ext extName multiple
  | os == "Linux" = do
    openDialog (head ext) (Just defaultPath)
  | otherwise = do
    path <- openFileDialog
      title
      (pack defaultPath)
      (map (\f -> pack $ "*." ++ f) ext)
      extName
      multiple
    case path of
      Nothing -> return Nothing
      Just path -> return (Just (unpack $ head path))

osSaveFileDialog :: Text -> Text -> String -> [String] -> Text -> IO (Maybe String)
osSaveFileDialog os title defaultPath ext extName
  | os == "Linux" = do
    openSaveDialog (head ext)
  | otherwise = do
    path <- saveFileDialog
      title
      (pack defaultPath)
      (map (\f -> pack $ "*." ++ f) ext)
      extName
    case path of
      Nothing -> return Nothing
      Just path -> return (Just (unpack path))

osSelectFolderDialog :: Text -> Text -> String -> IO (Maybe String)
osSelectFolderDialog os title defaultPath
  | os == "Linux" = do
    openFolderDialog
  | otherwise = do
    path <- selectFolderDialog
      title
      (pack defaultPath)
    case path of
      Nothing -> return Nothing
      Just path -> return (Just (unpack path))