{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.HandleEvent (
  handleEvent
) where

import Frontend.Types
import Frontend.Helper
import Frontend.Communication (startCommunication, evaluateProofString)
import Frontend.Parse
import Frontend.Preferences
import Frontend.Export (convertToLatex)
import Shared.Messages
import qualified Logic.Abs as Abs
import Logic.Par (pSequent, myLexer)

import Monomer
import Control.Lens
import Control.Exception (try, SomeException)
import Control.Concurrent (newChan)
import Control.Monad (filterM)
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (findIndex, isInfixOf)
import Data.Text (Text, unpack, pack, intercalate)
import TextShow ( TextShow(showt) )
import System.Directory ( doesFileExist, listDirectory, doesDirectoryExist, removeFile, createDirectoryIfMissing )
import System.FilePath ( takeExtension )

import NativeFileDialog ( openFolderDialog, openSaveDialog )
import qualified System.FilePath.Posix as FPP
import qualified Data.Map as Map

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  NoEvent -> []

  AppInit -> [
      Producer $ directoryFilesProducer (model ^. preferences . workingDir),
      Task $ do
        frontendChan <- newChan
        backendChan <- newChan
        answer <- startCommunication frontendChan backendChan
        return $ BackendResponse answer
    ]

  AppBeforeExit -> [
      cancelExitApplication,
      Producer (savePreferences model ExitApp)
    ]

  ExitApp -> [ exitApplication ]

  SetOpenMenuBarItem s -> [ Model $ model & openMenuBarItem .~ s ]

  FocusOnKey key -> [ SetFocusOnKey key ]

  NextFocus n -> replicate n (MoveFocusFromKey Nothing FocusFwd)

  AddPremise -> applyOnCurrentProof model addPremiseToProof ++ focusAction
    where
      focusAction = fromMaybe [] (getCurrentSequent model >>= Just . getFocusAction)
      getFocusAction sequent = [ SetFocusOnKey (WidgetKey $ "premise.input." <> showt idx) ]
        where idx = length (_premises sequent)

  RemovePremise idx -> applyOnCurrentProof model (removePremiseFromProof idx)

  EditPremise idx newText -> applyOnCurrentProof model (editPremisesInProof idx newText)

  EditConclusion newText -> applyOnCurrentProof model (editConclusionInProof newText)

  SwitchLineToSubProof path widgetKey -> applyOnCurrentProof model switch ++ [SetFocusOnKey widgetKey, MoveFocusFromKey Nothing FocusBwd]
    where
      switch = replaceInProof path (\oldLine -> SubProof [oldLine])

  SwitchSubProofToLine path widgetKey -> applyOnCurrentProof model switch ++ [SetFocusOnKey widgetKey, MoveFocusFromKey Nothing FocusFwd]
    where
      switch p = if not $ isSingleton $ evalPath p path then p else replaceInProof path (\oldLine -> case oldLine of
          SubProof p -> head p
          _ -> error ""
        ) p
      isSingleton (SubProof p) = length p == 1
      isSingleton _ = False

  InsertLineAfter path -> applyOnCurrentProof model insertLine ++ focusAction
    where
      focusAction = fromMaybe [] maybeFocusAction
      maybeFocusAction = (getCurrentSequent model >>= \f -> Just $ pathToLineNumber f path) >>= getFocusAction
      getFocusAction l = Just [ SetFocusOnKey (WidgetKey $ showt (l + 1) <> ".statement") ]
      insertLine = insertAfterProof path (Line "" "" 0 [])

  InsertSubProofAfter path -> applyOnCurrentProof model insertSubProof
    where insertSubProof = insertAfterProof path (SubProof [Line "" "" 0 []])

  AddLine -> applyOnCurrentProof model addLine
    where
      addLine sequent = FESequent premises conclusion steps
        where
          premises = _premises sequent
          conclusion = _conclusion sequent
          steps = _steps sequent ++ [Line "" "" 0 []]

  AddSubProof -> applyOnCurrentProof model addSubProof
    where
      addSubProof sequent = FESequent premises conclusion steps
        where
          premises = _premises sequent
          conclusion = _conclusion sequent
          steps = _steps sequent ++ [SubProof [Line "" "" 0 []]]

  RemoveLine path -> applyOnCurrentProof model removeLine ++ focusAction
    where
      removeLine = removeFromProof path

      focusAction = fromMaybe [] (getCurrentSequent model >>= Just . getFocusAction)
      getFocusAction sequent = [ SetFocusOnKey (WidgetKey $ showt l <> ".rule") ]
        where l = pathToLineNumber sequent path - 1

  EditFormula path newText -> applyOnCurrentProof model editFormula
    where editFormula = editFormulaInProof path newText

  EditRuleName path newText -> applyOnCurrentProof model editRuleName
    where editRuleName = editRuleNameInProof path newText

  EditRuleArgument path idx newText -> applyOnCurrentProof model editRuleArgument
    where editRuleArgument = editRuleArgumentInProof path idx newText

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

  ToggleRulesSidebar -> [ Model $ model & preferences . rulesSidebarOpen %~ toggle]
    where toggle = not

  ToggleFileExplorer -> [ Model $ model & preferences . fileExplorerOpen %~ toggle]
    where toggle = not

  RefreshExplorer -> [
      Model $ model & filesInDirectory .~ [],
      Producer $ directoryFilesProducer (model ^. preferences . workingDir)
    ]

  SetFilesInDirectory fs -> [ Model $ model & filesInDirectory .~ fs ]

  OpenPreferences -> handleEvent wenv node model (OpenFile_ preferencePath "")

  OpenGuide -> handleEvent wenv node model (OpenFile_ "user_guide_en.md" "./docs")

  OpenFile_ filePath folderPath -> [
      Producer (\sendMsg -> do
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
                where pParsedContent = Just (convertSeq seq_t)
        else
          sendMsg (OpenFileSuccess $ OtherFile fullPath pContentText)
      )
    ]
    where
      convertSeq (Abs.Seq premises conclusion proof) = FESequent (map convertForm premises) (convertForm conclusion) (convertProof proof)

      convertForm (Abs.FormNot a) = "!" <> getOutput a
        where
          getOutput form = case form of
            Abs.FormPred _ -> c
            Abs.FormNot _ -> c
            Abs.FormBot -> c
            Abs.FormPar _ -> c
            _ -> p
            where c = convertForm form; p = "(" <> c <> ")"

      convertForm (Abs.FormAnd a b) = getOutput a <> " & " <> getOutput b
        where
          getOutput form = case form of
            Abs.FormImpl _ _ -> p
            _ -> c
            where c = convertForm form; p = "(" <> c <> ")"

      convertForm (Abs.FormOr a b) = getOutput a <> " | " <> getOutput b
        where
          getOutput form = case form of
            Abs.FormImpl _ _ -> p
            _ -> c
            where c = convertForm form; p = "(" <> c <> ")"

      convertForm (Abs.FormImpl a b) = convertForm a <> " -> " <> convertForm b
      convertForm (Abs.FormPred (Abs.Pred (Abs.Ident i) _params)) = pack i
      convertForm (Abs.FormPar a) = "(" <> convertForm a <> ")"
      convertForm (Abs.FormEq a b) = convertTerm a <> "=" <> convertTerm b
      convertForm (Abs.FormAll (Abs.Ident i) a) = "all " <> pack i <> " " <> convertForm a
      convertForm (Abs.FormSome (Abs.Ident i) a) = "some " <> pack i <> " " <> convertForm a
      convertForm Abs.FormBot = "bot"
      convertForm Abs.FormNil = ""

      convertProof (Abs.Proof proofElems) = concat $ map convertProofElem proofElems
      convertProofElem (Abs.ProofElem _labels step) = convertStep step

      convertStep Abs.StepNil = [Line "" "" 0 []]
      convertStep (Abs.StepPrem _form) = [] 
      convertStep (Abs.StepAssume form) = [Line (convertForm form) "assume" 0 []]
      convertStep (Abs.StepProof proof) = [SubProof (convertProof proof)]
      convertStep (Abs.StepForm (Abs.Ident i) args form) = [Line (convertForm form) (pack i) (length args) (map convertArg args)]
      convertStep (Abs.StepFree (Abs.Ident i)) = [Line (pack i) "free" 0 []]

      convertTerm (Abs.Term (Abs.Ident i) (Abs.Params [])) = pack i
      convertTerm (Abs.Term (Abs.Ident i) (Abs.Params ts)) = pack i <> "(" <> intercalate ", " (map convertTerm ts) <> ")"

      convertArg (Abs.ArgLine i) = showt i
      convertArg (Abs.ArgRange a b) = showt a <> "-" <> showt b
      convertArg (Abs.ArgTerm t) = convertTerm t
      convertArg (Abs.ArgForm t f) = convertTerm t <> ":=" <> convertForm f

  OpenFile filePath -> handleEvent wenv node model (OpenFile_ filePath wd)
    where wd = fromMaybe "" (model ^. preferences . workingDir)

  OpenFileSuccess file -> Model newModel : handleEvent wenv node newModel (SetCurrentFile filePath)
    where
      newModel = model
        & preferences . openFiles %~ doOpenFile
        & preferences . tmpLoadedFiles %~ createNew file

      doOpenFile currentlyOpenFiles = currentlyOpenFiles ++ [filePath | filePath `notElem` model ^. preferences . openFiles]
      createNew newFile oldFiles = if _path newFile `elem` (model ^. preferences . openFiles) then
          oldFiles else
          filter (\f -> _path newFile /= _path f) oldFiles ++ [newFile]
      filePath = _path file

  CloseCurrentFile -> case model ^. currentFile of
    Nothing -> []
    Just filePath -> handleEvent wenv node model (CloseFile filePath)

  CloseFile filePath -> case file of
    Nothing -> []
    Just file -> (if isFileEdited (Just file) then [
        Model $ model
          & confirmDeletePopup .~ True
          & confirmDeleteTarget ?~ filePath
      ] else handleEvent wenv node model (CloseFileSuccess filePath))
    where file = getProofFileByPath (model ^. preferences . tmpLoadedFiles) filePath

  CloseFileSuccess filePath -> Model finalModel : deleteTmp
    where
      deleteTmp = [ Producer (\_ -> do
          result <- try (removeFile filePath) :: IO (Either SomeException ())
          case result of
            Left e -> print e
            Right _ -> return ()
        ) | "/_tmp/" `isInfixOf` filePath]
      finalModel = modelWithClosedFile
        & currentFile .~ (if cf == Just filePath then maybeHead (modelWithClosedFile ^. preferences . openFiles) else cf)
      modelWithClosedFile = model
        & preferences . openFiles %~ filter (filePath/=)
        & confirmDeleteTarget .~ Nothing
        & confirmDeletePopup .~ False
      cf = model ^. currentFile

  SaveCurrentFile -> case model ^. currentFile of
    Nothing -> []
    Just filePath -> case currentFile of
      Just file@ProofFile {} -> handleEvent wenv node model (SaveFile file)
      Just file@PreferenceFile {} -> handleEvent wenv node model (SaveFile file)
      _ -> []
      where currentFile = getProofFileByPath (model ^. preferences . tmpLoadedFiles) filePath

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
      getActions fileIndex = [ Model $ model & preferences . tmpLoadedFiles . singular (ix fileIndex) . isEdited .~ False ]
      fileIndex = getProofFileIndexByPath (model ^. preferences . tmpLoadedFiles) (_path f)

  SetCurrentFile filePath -> [
      Model $ model
        & currentFile ?~ filePath
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

  CheckCurrentProof -> case model ^. currentFile of
    Nothing -> []
    Just filePath -> case currentFile of
      Just file@ProofFile {} -> handleEvent wenv node model (CheckProof file)
      Just file@TemporaryProofFile {} -> handleEvent wenv node model (CheckProof file)
      _ -> []
      where currentFile = getProofFileByPath (model ^. preferences . tmpLoadedFiles) filePath

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
      Model $ model & preferences . workingDir .~ newWd,
      Producer (directoryFilesProducer newWd)
    ]
    where newWd = Just path

  ExportToLaTeX -> case model ^. currentFile of
    Nothing -> 
      [Message (WidgetKey "ExportError") (pack "Please save your proof first")]
    Just filePath -> case getProofFileByPath (model ^. preferences . tmpLoadedFiles) filePath of
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

  ExportSuccess msg -> [Message (WidgetKey "ExportSuccess") msg]
  ExportError msg -> [Message (WidgetKey "ExportError") msg]

  Print s -> [ Producer (\_ -> print s) ]
  f -> [ Producer (\_ -> print f) ]

directoryFilesProducer :: Maybe FilePath -> (AppEvent -> IO ()) -> IO ()
directoryFilesProducer workingDir sendMsg = do
  case workingDir of
    Nothing -> sendMsg (SetFilesInDirectory [])
    Just wd -> do
      result <- try (fmap (map (drop (length wd + 1))) (listDirectoryRecursive wd)) :: IO (Either SomeException [[Char]])
      case result of
        Left e -> do
          print e
          sendMsg (SetFilesInDirectory [])
        Right allFileNames -> sendMsg (SetFilesInDirectory allFileNames)

applyOnCurrentProof :: AppModel -> (FESequent -> FESequent) -> [EventResponse AppModel e sp ep]
applyOnCurrentProof model f = actions
  where
    actions = fromMaybe [] (fileIndex >>= Just . getActions)
    fileIndex = cf >>= getProofFileIndexByPath (model ^. preferences . tmpLoadedFiles)
    cf = model ^. currentFile
    getActions fileIndex = [
        Model $ model
          & preferences . tmpLoadedFiles . singular (ix fileIndex) . parsedSequent %~ maybeF
          & preferences . tmpLoadedFiles . singular (ix fileIndex) . isEdited .~ True
      ]
    maybeF (Just s) = Just (f s)
    maybeF Nothing = Nothing

addPremiseToProof :: FESequent -> FESequent
addPremiseToProof sequent = FESequent premises conclusion steps
  where
    premises = _premises sequent ++ [""]
    conclusion = _conclusion sequent
    steps = _steps sequent

removePremiseFromProof :: Int -> FESequent -> FESequent
removePremiseFromProof idx sequent = FESequent premises conclusion steps
  where
    premises = _premises sequent ^.. folded . ifiltered (\i _ -> i /= idx)
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

editFormulaInProof :: FormulaPath -> Text -> FESequent -> FESequent
editFormulaInProof path newText = replaceSteps f
  where
    f steps = zipWith (\p idx -> el path newText [idx] p) steps [0..]
    el editPath newText currentPath (SubProof p) = SubProof $ zipWith (\p idx -> el editPath newText (currentPath ++ [idx]) p) p [0..]
    el editPath newText currentPath f@(Line _statement rule usedArguments arguments)
      | editPath == currentPath = Line newText rule usedArguments arguments
      | otherwise = f

editRuleNameInProof :: FormulaPath -> Text -> FESequent -> FESequent
editRuleNameInProof path newText = replaceSteps f
  where
    f steps = zipWith (\p idx -> el path newText [idx] p) steps [0..]
    el editPath newText currentPath (SubProof p) = SubProof $ zipWith (\p idx -> el editPath newText (currentPath ++ [idx]) p) p [0..]
    el editPath newText currentPath f@(Line statement _rule usedArguments arguments)
      | editPath == currentPath = case Map.lookup newText ruleMetaDataMap of
        Nothing -> Line statement newText usedArguments arguments
        Just (RuleMetaData nrArguments _) -> Line statement newText (fromIntegral nrArguments) (fillList nrArguments arguments)
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

removeFromProof :: FormulaPath -> FESequent -> FESequent
removeFromProof path = replaceSteps f
  where
    f steps = if null res then startProof else res
      where
        startProof = [Line "" "" 0 []]
        res = filterValid $ zipWith (\p idx -> rl path [idx] p) steps [0..]
    rl removePath currentPath (SubProof p)
      | removePath == currentPath = Nothing
      | otherwise = Just $ SubProof $ filterValid $ zipWith (\p idx -> rl removePath (currentPath ++ [idx]) p) p [0..]
    rl removePath currentPath f@(Line {})
      | removePath == currentPath = Nothing
      | otherwise = Just f

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
    fileIndex = cf >>= getProofFileIndexByPath (model ^. preferences . tmpLoadedFiles)
    cf = model ^. currentFile

    getSequent fileIndex = case model ^. preferences . tmpLoadedFiles . singular (ix fileIndex) of
      f@ProofFile {} -> _parsedSequent f
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