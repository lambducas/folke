{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.HandleEvent (
  handleEvent
) where

import Frontend.Types
import Frontend.Helper
import Frontend.Communication (startCommunication, evaluateProofString)
import Frontend.Parse
import Frontend.Preferences
import Frontend.Export (convertToLatex, compileLatexToPDF)
import Frontend.History
import Shared.Messages
import Logic.Par (pSequent, myLexer, pArg)
import qualified Logic.Abs as Abs

import Monomer
import Control.Lens
import Control.Exception (try, SomeException)
import Control.Concurrent (newChan)
import Control.Monad (filterM)
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (findIndex, isInfixOf)
import Data.Text (Text, unpack, pack)
import TextShow ( TextShow(showt) )
import System.Directory ( doesFileExist, listDirectory, doesDirectoryExist, removeFile, createDirectoryIfMissing )
import System.FilePath ( takeExtension, dropExtension)

import NativeFileDialog ( openFolderDialog, openSaveDialog )
import qualified System.FilePath.Posix as FPP
import qualified Data.Map as Map

import qualified SDL

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  NoEvent -> []

  Undo ->
   if model ^. historyIndex > 0 && not (null $ model ^. stateHistory) then
    let prevModel = (model ^. stateHistory) !! (model ^. historyIndex - 1)
        newModel = prevModel
          & stateHistory .~ model ^. stateHistory
          & historyIndex .~ model ^. historyIndex - 1
          & ignoreHistoryOnce .~ True
    in [Model newModel]
  else []

  Redo -> undefined

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

  AddPremise idx -> applyOnCurrentProof model (addPremiseToProof (idx + 1)) ++ [ SetFocusOnKey (WidgetKey $ "premise.input." <> showt (idx + 1)) ]

  RemovePremise idx -> applyOnCurrentProof model (removePremiseFromProof idx)

  EditPremise idx newText -> applyOnCurrentProof model (editPremisesInProof idx newText)

  EditConclusion newText -> applyOnCurrentProof model (editConclusionInProof newText)

  SwitchLineToSubProof path widgetKey -> applyOnCurrentProof model switch ++ [SetFocusOnKey widgetKey]
    where
      switch = replaceInProof path (\oldLine -> SubProof [oldLine])

  SwitchSubProofToLine path widgetKey -> applyOnCurrentProof model switch ++ [SetFocusOnKey widgetKey]
    where
      switch p = if not $ isSingleton $ evalPath p path then p else replaceInProof path (\oldLine -> case oldLine of
          SubProof p -> head p
          _ -> error ""
        ) p
      isSingleton (SubProof p) = length p == 1
      isSingleton _ = False

  InsertLineAfter updateRef path -> applyOnCurrentProof model (insertAfterAndMaybeUpdateRefs path insertThis updateRef) ++ focusAction
    where
      insertThis = Line "" "" 0 []
      focusAction = fromMaybe [] maybeFocusAction
      maybeFocusAction = (getCurrentSequent model >>= \f -> Just $ pathToLineNumber f nextPath) >>= getFocusAction
      getFocusAction l = Just [ SetFocusOnKey (WidgetKey $ showt l <> ".statement") ]
      nextPath = init path ++ [last path + 1]

  InsertLineBefore updateRef path -> applyOnCurrentProof model (insertBeforeAndMaybeUpdateRefs path insertThis updateRef) ++ focusAction
    where
      insertThis = Line "" "" 0 []
      focusAction = fromMaybe [] maybeFocusAction
      maybeFocusAction = (getCurrentSequent model >>= \f -> Just $ pathToLineNumber f nextPath) >>= getFocusAction
      getFocusAction l = Just [ SetFocusOnKey (WidgetKey $ showt l <> ".statement") ]
      nextPath = init path ++ [last path + 0]

  InsertSubProofAfter updateRef path -> applyOnCurrentProof model (insertAfterAndMaybeUpdateRefs path insertThis updateRef)
    where insertThis = SubProof [Line "" "" 0 []]

  InsertSubProofBefore updateRef path -> applyOnCurrentProof model (insertBeforeAndMaybeUpdateRefs path insertThis updateRef)
    where insertThis = SubProof [Line "" "" 0 []]

  AddLine -> applyOnCurrentProof model insertLine
    where insertLine seq = insertAfterProof (pathToLastLine seq) (Line "" "" 0 []) seq

  AddSubProof -> applyOnCurrentProof model insertSubProof
    where insertSubProof seq = insertAfterProof (pathToLastLine seq) (SubProof [Line "" "" 0 []]) seq

  RemoveLine updateRef path -> applyOnCurrentProof model removeLine ++ focusAction
    where
      removeLine = removeFromProof path True . offsetFunc
      offsetFunc seq = if updateRef then offsetAllRefs (-1) lineNumber seq else seq
        where lineNumber = pathToLineNumberOffsetPremises seq path + 1

      focusAction = fromMaybe [] (getCurrentSequent model >>= Just . getFocusAction)
      getFocusAction sequent = [ SetFocusOnKey (WidgetKey $ showt l <> ".statement") ]
        where l = pathToLineNumber sequent path - 1

  EditFormula path newText -> applyOnCurrentProof model editFormula
    where editFormula = editFormulaInProof path newText

  EditRuleName path newText -> applyOnCurrentProof model editRuleName
    where editRuleName = editRuleNameInProof path newText

  EditRuleArgument path idx newText -> applyOnCurrentProof model editRuleArgument
    where editRuleArgument = editRuleArgumentInProof path idx newText

  MovePathToPath target dest
    | pathIsParentOf target dest -> []
    | otherwise -> applyOnCurrentProof model f
    where
      f seq
        | target > dest = (removeInvalidFromProof . copyTarget seq . deleteTarget) seq
        | otherwise     = (removeInvalidFromProof . deleteTarget . copyTarget seq) seq
      copyTarget oldestSeq = insertAfterProof dest (evalPath oldestSeq target)
      deleteTarget = removeFromProof target False

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
            let file = TemporaryProofFile randomPath emptySeq False
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

        if takeExtension fullPath == ".md" then
          sendMsg (OpenFileSuccess $ MarkdownFile fullPath pContentText)
        else if fullPath == preferencePath && folderPath == "" then
          sendMsg (OpenFileSuccess $ PreferenceFile fullPath pIsEdited)
        else if takeExtension fullPath == "." <> feFileExt then
          do
            let seq = parseProofFromJSON pContentText
            sendMsg (OpenFileSuccess $ ProofFile fullPath pContentText seq pIsEdited)
        else if takeExtension fullPath == ".logic" then
          do
            case pSequent (myLexer pContent) of
              Left _err -> do
                case parseProofFromSimpleFileFormat pContentText of
                  seq@(Just _) -> sendMsg (OpenFileSuccess $ ProofFile fullPath pContentText seq pIsEdited)
                  Nothing -> sendMsg (OpenFileSuccess $ ProofFile fullPath pContentText (parseProofFromJSON pContentText) pIsEdited)

              Right seq_t -> sendMsg (OpenFileSuccess $ ProofFile fullPath pContentText pParsedContent pIsEdited)
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


applyOnCurrentProof :: AppModel -> (FESequent -> FESequent) -> [EventResponse AppModel AppEvent sp ep]
applyOnCurrentProof model f = actions
  where
    actions = fromMaybe [] (fileIndex >>= Just . getActions)
    fileIndex = cf >>= getProofFileIndexByPath (model ^. persistentState . tmpLoadedFiles)
    cf = model ^. persistentState . currentFile
    getActions fileIndex = [
        Model $ saveModelToHistory model $ model  -- Already using saveModelToHistory correctly
          & persistentState . tmpLoadedFiles . singular (ix fileIndex) . parsedSequent %~ maybeF
          & persistentState . tmpLoadedFiles . singular (ix fileIndex) . isEdited .~ True
          & proofStatus .~ Nothing
      ]
    maybeF (Just s) = Just (f s)
    maybeF Nothing = Nothing

addPremiseToProof :: Int -> FESequent -> FESequent
addPremiseToProof idx sequent = FESequent premises conclusion (steps' sequent)
  where
    premises = insertAt "" idx (_premises sequent)
    conclusion = _conclusion sequent
    steps' seq = _steps (offsetAllRefs 1 (toInteger idx+1) seq)

removePremiseFromProof :: Int -> FESequent -> FESequent
removePremiseFromProof idx sequent = FESequent premises conclusion (steps' sequent)
  where
    premises = _premises sequent ^.. folded . ifiltered (\i _ -> i /= idx)
    conclusion = _conclusion sequent
    steps' seq = _steps (offsetAllRefs (-1) (toInteger idx+1) seq)

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

editFormulaInProof :: FormulaPath -> Text -> FESequent -> FESequent
editFormulaInProof path newText = replaceSteps f
  where
    f steps = zipWith (\p idx -> el path newText [idx] p) steps [0..]
    el editPath newText currentPath (SubProof p) = SubProof $ zipWith (\p idx -> el editPath newText (currentPath ++ [idx]) p) p [0..]
    el editPath newText currentPath f@(Line _statement rule usedArguments arguments)
      | editPath == currentPath = Line newText rule usedArguments arguments
      | otherwise = f

editRuleNameInProof :: FormulaPath -> Text -> FESequent -> FESequent
editRuleNameInProof path newRule = replaceSteps f
  where
    f steps = zipWith (\p idx -> el path [idx] p) steps [0..]
    el editPath currentPath (SubProof p) = SubProof $ zipWith (\p idx -> el editPath (currentPath ++ [idx]) p) p [0..]
    el editPath currentPath f@(Line statement _rule usedArguments arguments)
      | editPath == currentPath = case Map.lookup (parseRule newRule) ruleMetaDataMap of
        Nothing -> Line statement newRule usedArguments arguments
        Just (RuleMetaData nrArguments _) -> Line statement newRule (fromIntegral nrArguments) (fillList nrArguments arguments)
      | otherwise = f

    fillList :: Integer -> [Text] -> [Text]
    fillList targetLen arr
      | currLen < targetLen = arr ++ replicate (fromIntegral targetLen - length arr) ""
      | otherwise = arr
      where currLen = toInteger (length arr)

editRuleArgumentInProof :: FormulaPath -> Int -> Text -> FESequent -> FESequent
editRuleArgumentInProof path idx newText = replaceSteps f
  where
    f steps = zipWith (\p idx -> el path newText [idx] p) steps [0..]
    el editPath newText currentPath (SubProof p) = SubProof $ zipWith (\p idx -> el editPath newText (currentPath ++ [idx]) p) p [0..]
    el editPath newText currentPath f@(Line statement rule usedArguments arguments)
      | editPath == currentPath = Line statement rule usedArguments (arguments & element idx .~ newText)
      | otherwise = f

replaceInProof :: FormulaPath -> (FEStep -> FEStep) -> FESequent -> FESequent
replaceInProof path replaceWith = replaceSteps f
  where
    f steps = zipWith (\p idx -> rl [idx] p) steps [0..]
    rl currentPath f@(SubProof p)
      | path == currentPath = replaceWith f
      | otherwise = SubProof $ zipWith (\p idx -> rl (currentPath ++ [idx]) p) p [0..]
    rl currentPath f@(Line {})
      | path == currentPath = replaceWith f
      | otherwise = f

insertAfterAndMaybeUpdateRefs :: FormulaPath -> FEStep -> Bool -> FESequent -> FESequent
insertAfterAndMaybeUpdateRefs path insertThis updateRef = insertAfterProof path insertThis . offsetFunc
  where
    offsetFunc seq = if updateRef then offsetAllRefs 1 lineNumber seq else seq
      where
        d = evalPath seq path
        isSubProof (SubProof {}) = True
        isSubProof _ = False
        lineNumber
          | isSubProof d = pathToLineNumberOffsetPremises seq path + proofStepLength d
          | otherwise = pathToLineNumberOffsetPremises seq path + 1

insertBeforeAndMaybeUpdateRefs :: FormulaPath -> FEStep -> Bool -> FESequent -> FESequent
insertBeforeAndMaybeUpdateRefs path insertThis updateRef = insertBeforeProof path insertThis . offsetFunc
  where
    offsetFunc seq = if updateRef then offsetAllRefs 1 lineNumber seq else seq
      where lineNumber = pathToLineNumberOffsetPremises seq path

insertAfterProof :: FormulaPath -> FEStep -> FESequent -> FESequent
insertAfterProof path insertThis = replaceSteps f
  where
    f steps = concat (zipWith (\p idx -> rl [idx] p) steps [0..])
    rl currentPath (SubProof p)
      | path == currentPath = [res, insertThis]
      | otherwise = [res]
        where res = SubProof $ concat $ zipWith (\p idx -> rl (currentPath ++ [idx]) p) p [0..]
    rl currentPath f@(Line {})
      | path == currentPath = [f, insertThis]
      | otherwise = [f]

insertBeforeProof :: FormulaPath -> FEStep -> FESequent -> FESequent
insertBeforeProof path insertThis = replaceSteps f
  where
    f steps = concat (zipWith (\p idx -> rl [idx] p) steps [0..])
    rl currentPath (SubProof p)
      | path == currentPath = [insertThis, res]
      | otherwise = [res]
        where res = SubProof $ concat $ zipWith (\p idx -> rl (currentPath ++ [idx]) p) p [0..]
    rl currentPath f@(Line {})
      | path == currentPath = [insertThis, f]
      | otherwise = [f]

removeFromProof :: FormulaPath -> Bool -> FESequent -> FESequent
removeFromProof path removeInvalid
  | removeInvalid = removeInvalidFromProof . replaceSteps f
  | otherwise = replaceSteps f
  where
    f steps = catMaybes $ zipWith (\p idx -> rl path [idx] p) steps [0..]
    rl removePath currentPath (SubProof p)
      | removePath == currentPath = Nothing
      | otherwise = Just $ SubProof $ catMaybes $ zipWith (\p idx -> rl removePath (currentPath ++ [idx]) p) p [0..]
    rl removePath currentPath f@(Line {})
      | removePath == currentPath = Nothing
      | otherwise = Just f

removeInvalidFromProof :: FESequent -> FESequent
removeInvalidFromProof = replaceSteps f
  where
    f steps = if null res then startProof else res
      where
        startProof = [Line "" "" 0 []]
        res = filterValid $ map rl steps
    rl (SubProof p) = Just $ SubProof $ filterValid $ map rl p
    rl f@(Line {}) = Just f

    filterValid = filter validateProof . catMaybes
    validateProof (SubProof []) = False
    validateProof _ = True

replaceSteps :: ([FEStep] -> [FEStep]) -> FESequent -> FESequent
replaceSteps f sequent = FESequent premises conclusion steps
  where
    premises = _premises sequent
    conclusion = _conclusion sequent
    steps = f $ _steps sequent

getCurrentSequent :: AppModel -> Maybe FESequent
getCurrentSequent model = sequent
  where
    sequent = fileIndex >>= getSequent
    fileIndex = cf >>= getProofFileIndexByPath (model ^. persistentState . tmpLoadedFiles)
    cf = model ^. persistentState . currentFile

    getSequent fileIndex = case model ^. persistentState . tmpLoadedFiles . singular (ix fileIndex) of
      f@ProofFile {} -> _parsedSequent f
      f@TemporaryProofFile {} -> _parsedSequent f
      _ -> Nothing

getProofFileIndexByPath :: [File] -> FilePath -> Maybe Int
getProofFileIndexByPath allFiles filePath = findIndex (\f -> _path f == filePath) allFiles

evaluateCurrentProof :: AppModel -> File -> (AppEvent -> IO ()) -> IO ()
evaluateCurrentProof model file sendMsg = do
  case _parsedSequent file of
    Nothing -> return ()
    Just seq -> do
      let text = unpack $ parseProofForBackend seq
      putStrLn text
      answer <- evaluateProofString (model ^. frontendChan) (model ^. backendChan) text
      sendMsg (BackendResponse answer)

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

offsetAllRefs :: Integer -> Integer -> FESequent -> FESequent
offsetAllRefs by after sequent = applyOnAllRefs f sequent
  where f = offsetLineRefBy by after

applyOnAllRefs :: (Text -> Text) -> FESequent -> FESequent
applyOnAllRefs func = replaceSteps (map f)
  where
    f (SubProof p) = SubProof (map f p)
    f (Line s r u a) = Line s r u (map func a)

offsetLineRefBy :: Integer -> Integer -> Text -> Text
offsetLineRefBy by after = applyOnLineNumberRef f
  where f l
          | l >= after = l + by
          | otherwise = l

applyOnLineNumberRef :: (Integer -> Integer) -> Text -> Text
applyOnLineNumberRef f refText = case pArg (myLexer (unpack refText)) of
  Left {} -> refText
  Right (Abs.ArgRange a b) -> showt (f a) <> "-" <> showt (f b)
  Right (Abs.ArgLine l) -> showt (f l)
  Right _ -> refText