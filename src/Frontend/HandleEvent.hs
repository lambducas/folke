{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.HandleEvent (
  handleEvent
) where

import Frontend.Types
import Frontend.Helper.General
import Frontend.Helper.ProofHelper
import Frontend.Communication (startCommunication)
import Frontend.Parse
import Frontend.Preferences
import Frontend.Export (convertToLatex, compileLatexToPDF)
import Frontend.History ( applyRedo, applyUndo )
import Shared.Messages
import Logic.Par (pSequent, myLexer)

import Monomer
import Control.Lens
import Control.Exception (try, SomeException)
import Control.Concurrent (newChan)
import Data.Maybe (fromMaybe)
import Data.List (isInfixOf)
import Data.Text (unpack, pack)
import TextShow ( TextShow(showt) )
import System.Directory ( removeFile, createDirectoryIfMissing )
import System.FilePath ( takeExtension, dropExtension)

import NativeFileDialog ( openFolderDialog, openSaveDialog )
import qualified System.FilePath.Posix as FPP

import qualified SDL

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  NoEvent -> []

  Undo -> applyOnCurrentFile model applyUndo

  Redo -> applyOnCurrentFile model applyRedo

  -- Undo ->
  --  if model ^. historyIndex > 0 && not (null $ model ^. stateHistory) then
  --   let prevModel = (model ^. stateHistory) !! (model ^. historyIndex - 1)
  --       newModel = prevModel
  --         & stateHistory .~ model ^. stateHistory
  --         & historyIndex .~ model ^. historyIndex - 1
  --         & ignoreHistoryOnce .~ True
  --   in [Model newModel]
  -- else []

  AppInit -> [
      Producer $ directoryFilesProducer (model ^. persistentState . workingDir),
      Task $ do
        frontendChan <- newChan
        backendChan <- newChan
        answer <- startCommunication frontendChan backendChan
        return $ BackendResponse answer
    ]

  AppBeforeExit -> [
      cancelExitApplication,
      Producer (savePrefAndState model ExitApp)
    ]

  ExitApp -> [ exitApplication ]

  AppResize m -> [ Model $ model & persistentState . windowMode .~ m ]

  CopyToClipboard t -> [ Producer (\_ -> SDL.setClipboardText t)]

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

  AddPremise idx -> applyOnCurrentProofAndRecordSequentHistory model (addPremiseToProof (idx + 1) "") ++ [ SetFocusOnKey (WidgetKey $ "premise.input." <> showt (idx + 1)) ]
    -- where f seq = (addPremiseToProof (idx + 1) "" seq, HInsertPremise (idx + 1) "")

  RemovePremise idx -> applyOnCurrentProofAndRecordSequentHistory model (removePremiseFromProof idx)
    -- where f seq = (removePremiseFromProof idx seq, HRemovePremise idx (_premises seq !! idx))

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
      maybeFocusAction = (getCurrentSequent model >>= \f -> Just $ pathToLineNumber f nextPath) >>= getFocusAction
      getFocusAction l = Just [ SetFocusOnKey (WidgetKey $ showt l <> ".statement") ]
      nextPath = init path ++ [last path + 1]

  InsertLineBefore updateRef path -> applyOnCurrentProofAndRecordSequentHistory model f ++ focusAction
    where
      f = insertBeforeAndMaybeUpdateRefs path insertThis updateRef
      -- f seq = (insertBeforeAndMaybeUpdateRefs path insertThis updateRef seq, HInsertStep updateRef nextPath insertThis)
      insertThis = Line "" "" 0 []
      focusAction = fromMaybe [] maybeFocusAction
      maybeFocusAction = (getCurrentSequent model >>= \f -> Just $ pathToLineNumber f nextPath) >>= getFocusAction
      getFocusAction l = Just [ SetFocusOnKey (WidgetKey $ showt l <> ".statement") ]
      nextPath = init path ++ [last path + 0]

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
      offsetFunc seq = if updateRef then offsetAllRefs (-1) lineNumber True seq else seq
        where lineNumber = pathToLineNumberOffsetPremises seq path + 1

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

  EditFormula path newText -> editLineAndRecordHistory model path (editFormulaInProof path newText)

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
        createDirectoryIfMissing False "./_tmp"
        let randomPath = ("./_tmp/" <> randomFileName) FPP.<.> "tmp"

        result <- try (writeFile randomPath "") :: IO (Either SomeException ())
        case result of
          Left e -> print e
          Right _ -> do
            let emptySeq = Just $ FESequent [] "" [Line "" "" 0 []]
            let emptyHistory = History {
              _hState = [],
              _hIndex = -1
            }
            let file = TemporaryProofFile randomPath emptySeq False emptyHistory
            sendMsg (OpenFileSuccess file)
      )
    ]

  ToggleRulesSidebar -> [ Model $ model & persistentState . rulesSidebarOpen %~ toggle]
    where toggle = not

  ToggleFileExplorer -> [ Model $ model & persistentState . fileExplorerOpen %~ toggle]
    where toggle = not

  RefreshExplorer -> [
      Model $ model & filesInDirectory .~ Just [],
      Producer $ directoryFilesProducer (model ^. persistentState . workingDir)
    ]

  SetFilesInDirectory fs -> [ Model $ model & filesInDirectory .~ fs ]

  OpenPreferences -> [ Producer (\sendMsg -> do
      preferencePath <- getPreferencePath
      sendMsg (OpenFile_ preferencePath "")
    )]

  OpenGuide -> handleEvent wenv node model (OpenFile_ "user_guide_en.md" "./docs")

  OpenFile_ filePath folderPath -> [
      Producer (\sendMsg -> do
        preferencePath <- getPreferencePath
        let fullPath = folderPath FPP.</> filePath
        pContent <- readFile fullPath
        let pContentText = pack pContent
        let pIsEdited = False
        let pHistory = History {
          _hState = [],
          _hIndex = -1
        }

        if takeExtension fullPath == ".md" then
          sendMsg (OpenFileSuccess $ MarkdownFile fullPath pContentText)
        else if fullPath == preferencePath && folderPath == "" then
          sendMsg (OpenFileSuccess $ PreferenceFile fullPath pIsEdited)
        else if takeExtension fullPath == "." <> feFileExt then
          do
            let seq = parseProofFromJSON pContentText
            sendMsg (OpenFileSuccess $ ProofFile fullPath pContentText seq pIsEdited pHistory)
        else if takeExtension fullPath == ".logic" then
          do
            case pSequent (myLexer pContent) of
              Left _err -> do
                case parseProofFromSimpleFileFormat pContentText of
                  seq@(Just _) -> sendMsg (OpenFileSuccess $ ProofFile fullPath pContentText seq pIsEdited pHistory)
                  Nothing -> sendMsg (OpenFileSuccess $ ProofFile fullPath pContentText (parseProofFromJSON pContentText) pIsEdited pHistory)

              Right seq_t -> sendMsg (OpenFileSuccess $ ProofFile fullPath pContentText pParsedContent pIsEdited pHistory)
                where pParsedContent = Just (convertBESequentToFESequent seq_t)
        else
          sendMsg (OpenFileSuccess $ OtherFile fullPath pContentText)
      )
    ]

  OpenFile filePath -> handleEvent wenv node model (OpenFile_ filePath wd)
    where wd = fromMaybe "" (model ^. persistentState . workingDir)

  OpenFileSuccess file -> Model newModel : handleEvent wenv node newModel (SetCurrentFile filePath)
    where
      newModel = model
        & persistentState . openFiles %~ doOpenFile
        & persistentState . tmpLoadedFiles %~ createNew file

      doOpenFile currentlyOpenFiles = currentlyOpenFiles ++ [filePath | filePath `notElem` model ^. persistentState . openFiles]
      createNew newFile oldFiles = if _path newFile `elem` (model ^. persistentState . openFiles) then
          oldFiles else
          filter (\f -> _path newFile /= _path f) oldFiles ++ [newFile]
      filePath = _path file

  CloseCurrentFile -> case model ^. persistentState . currentFile of
    Nothing -> []
    Just filePath -> handleEvent wenv node model (CloseFile filePath)

  CloseFile filePath -> case file of
    Nothing -> []
    Just file -> (if isFileEdited (Just file)
      then handleEvent wenv node model (OpenConfirmAction (ConfirmActionData {
        _cadTitle = "Close without saving?",
        _cadBody = "Are you sure you want to close\n" <> pack filePath <> "\nwithout saving? All changes will be lost!",
        _cadAction = CloseFileSuccess filePath
      }))
      else handleEvent wenv node model (CloseFileSuccess filePath))
    where file = getProofFileByPath (model ^. persistentState . tmpLoadedFiles) filePath

  CloseFileSuccess filePath -> Model finalModel : deleteTmp
    where
      deleteTmp = [ Producer (\_ -> do
          result <- try (removeFile filePath) :: IO (Either SomeException ())
          case result of
            Left e -> print e
            Right _ -> return ()
        ) | "/_tmp/" `isInfixOf` filePath]
      finalModel = modelWithClosedFile
        & persistentState . currentFile .~ (if cf == Just filePath then maybeHead (modelWithClosedFile ^. persistentState . openFiles) else cf)
      modelWithClosedFile = model
        & persistentState . openFiles %~ filter (filePath/=)
      cf = model ^. persistentState . currentFile

  SaveCurrentFile -> case model ^. persistentState . currentFile of
    Nothing -> []
    Just filePath -> case currentFile of
      Just file@ProofFile {} -> handleEvent wenv node model (SaveFile file)
      Just file@TemporaryProofFile {} -> handleEvent wenv node model (SaveFile file)
      Just file@PreferenceFile {} -> handleEvent wenv node model (SaveFile file)
      _ -> []
      where currentFile = getProofFileByPath (model ^. persistentState . tmpLoadedFiles) filePath

  SaveFile f -> case f of
    PreferenceFile {} -> handleEvent wenv node model SavePreferences
    f@ProofFile {} -> case _parsedSequent f of
      Nothing -> []
      Just seq -> [
          Producer (\sendMsg -> do
            let content = (unpack . parseProofToJSON) seq
                fileName = _path f

            result <- try (writeFile fileName content) :: IO (Either SomeException ())
            case result of
              Left e -> print e
              Right _ -> sendMsg (SaveFileSuccess f)
          )
        ]
    f@TemporaryProofFile {} -> case _parsedSequent f of
      Nothing -> []
      Just seq -> [
          Producer (\sendMsg -> do
            mNewPath <- openSaveDialog
            case mNewPath of
              Nothing -> return ()
              Just newPath -> do
                let content = (unpack . parseProofToJSON) seq
                result <- try (writeFile newPath content) :: IO (Either SomeException ())
                case result of
                  Left e -> print e
                  Right _ -> do
                    let tmpPath = _path f
                    sendMsg (SaveFileSuccess f)
                    sendMsg (CloseFileSuccess tmpPath)
                    sendMsg RefreshExplorer
                    sendMsg (OpenFile_ newPath "")
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
        & proofStatus .~ Nothing
    ]

  SwitchTheme -> [
      Model $ model & preferences . selectedTheme %~ switchTheme
    ]
    where
      switchTheme Light = Dark
      switchTheme Dark = Light

  UpdateFont s -> [Model $ model & preferences . normalFont .~ head s]
  ResetFontSize -> [Model $ model & preferences . fontSize .~ 16]

  ReadPreferences -> [ Producer readAndApplyPreferences ]
  ReadPreferences_ prefs -> [ Model $ model & preferences .~ prefs ]

  SavePreferences -> [ Producer (savePreferences model NoEvent) ]

  CheckCurrentProof -> case model ^. persistentState . currentFile of
    Nothing -> []
    Just filePath -> case currentFile of
      Just file@ProofFile {} -> handleEvent wenv node model (CheckProof file)
      Just file@TemporaryProofFile {} -> handleEvent wenv node model (CheckProof file)
      _ -> []
      where currentFile = getProofFileByPath (model ^. persistentState . tmpLoadedFiles) filePath

  CheckProof file -> [
      Model $ model & proofStatus .~ Nothing,
      Producer (evaluateCurrentProof model file)
    ]

  BackendResponse (StringSequentChecked result) -> [ Model $ model & proofStatus ?~ result ]
  BackendResponse (SequentChecked result) -> [ Model $ model & proofStatus ?~ result ]

  BackendResponse (OtherBackendMessage message) -> [ Producer (\_ -> print $ "From backend: " ++ message) ]

  OpenSetWorkingDir -> [ Producer openDiag ]
    where
      openDiag sendMsg = do
        path <- openFolderDialog
        case path of
          Nothing -> return ()
          Just path -> sendMsg (SetWorkingDir path)

  SetWorkingDir path -> [
      Model $ model & persistentState . workingDir .~ newWd,
      Producer (directoryFilesProducer newWd)
    ]
    where newWd = Just path

  ExportToLaTeX -> case model ^. persistentState . currentFile of
    Nothing ->
      [Message (WidgetKey "ExportError") (pack "Please save your proof first")]
    Just filePath -> case getProofFileByPath (model ^. persistentState . tmpLoadedFiles) filePath of
      Just file@ProofFile{} -> case _parsedSequent file of
        Nothing -> [Message (WidgetKey "ExportError") (pack "Cannot export invalid proof")]
        Just _ ->
          [ Producer (\sendMsg -> do
              -- Open a save dialog to let the user choose where to save the LaTeX file
              mSavePath <- openSaveDialog
              case mSavePath of
                Nothing ->
                  -- User cancelled the dialog
                  sendMsg (ExportError "Export cancelled")
                Just savePath -> do
                  -- Generate proper LaTeX content using our export module
                  let texPath = if takeExtension savePath == ".tex"
                                then savePath
                                else savePath <> ".tex"
                      latexContent = convertToLatex model

                  -- Write the full LaTeX content to the file
                  writeFile texPath (unpack latexContent)
                  putStrLn $ "Exported LaTeX file to: " ++ texPath
                  sendMsg (ExportSuccess (pack ("LaTeX file created at: " ++ texPath)))
            )
          ]
      _ -> [Message (WidgetKey "ExportError") (pack "Only proof files can be exported")]

  ExportToPDF -> case model ^. persistentState . currentFile of
    Nothing ->
      [Message (WidgetKey "ExportError") (pack "Please save your proof first")]
    Just filePath -> case getProofFileByPath (model ^. persistentState . tmpLoadedFiles) filePath of
      Just file@ProofFile{} -> case _parsedSequent file of
        Nothing -> [Message (WidgetKey "ExportError") (pack "Cannot export invalid proof")]
        Just _ ->
          [ Producer (\sendMsg -> do
              -- Open a save dialog to let the user choose where to save the file
              mSavePath <- openSaveDialog
              case mSavePath of
                Nothing -> return ()
                Just savePath -> do
                  -- Generate LaTeX content
                  let basePath = if takeExtension savePath == ".pdf"
                                 then dropExtension savePath
                                 else savePath
                      texPath = basePath <> ".tex"
                      latexContent = convertToLatex model

                  -- Write the LaTeX content to the file
                  writeFile texPath (unpack latexContent)

                  -- Compile the LaTeX to PDF (aux/log files will be in temp dir)
                  result <- compileLatexToPDF texPath
                  case result of
                    Right pdfPath -> do
                      sendMsg (ExportSuccess (pack $ "Files created: " ++ texPath ++ " and " ++ pdfPath))
                    Left err -> do
                      sendMsg (ExportError (pack $ "PDF compilation failed: " ++ err))
            )
          ]
      _ -> [Message (WidgetKey "ExportError") (pack "Only proof files can be exported")]

  ExportSuccess msg -> [Message (WidgetKey "ExportSuccess") msg]
  ExportError msg -> [Message (WidgetKey "ExportError") msg]

  Print s -> [ Producer (\_ -> print s) ]
  f -> [ Producer (\_ -> print f) ]

{-|
Recursively gets all files in working directory and
sends back `SetFilesInDirectory` event with the files
-}
directoryFilesProducer :: Maybe FilePath -> (AppEvent -> IO ()) -> IO ()
directoryFilesProducer workingDir sendMsg = do
  case workingDir of
    Nothing -> sendMsg (SetFilesInDirectory Nothing)
    Just wd -> do
      result <- try (fmap (map (drop (length wd + 1))) (listDirectoryRecursive wd)) :: IO (Either SomeException [[Char]])
      case result of
        Left e -> do
          print e
          sendMsg (SetFilesInDirectory Nothing)
        Right allFileNames -> sendMsg (SetFilesInDirectory (Just allFileNames))