{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.HandleEvent (
  handleEvent
) where

import Frontend.Types
import Frontend.Helper
import Frontend.Themes
import Frontend.Communication (startCommunication, evaluateProofString)
import Shared.Messages
import qualified Logic.Abs as Abs
import Logic.Par (pSequent, myLexer)

import Monomer
import Control.Lens
import Control.Exception (try, SomeException)
import Control.Concurrent (newChan)
import Control.Monad (filterM)
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (findIndex)
import Data.Text (Text, unpack, pack, intercalate)
import TextShow ( TextShow(showt) )
import System.Directory ( doesFileExist, listDirectory, doesDirectoryExist )
import System.FilePath ( takeExtension )

import NativeFileDialog ( openFolderDialog )
import System.FilePath.Posix ((</>))

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  NoEvent -> []

  AppInit -> [
      Producer $ directoryFilesProducer (model ^. workingDir),
      Task $ do
        frontendChan <- newChan
        backendChan <- newChan
        answer <- startCommunication frontendChan backendChan
        return $ BackendResponse answer
    ]

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

  SwitchLineToSubProof path widgetKey -> applyOnCurrentProof model switch ++ [SetFocusOnKey widgetKey, MoveFocusFromKey Nothing FocusBwd]-- focusAction
    where
      switch = replaceInProof path (\oldLine -> SubProof [oldLine])

      focusAction = fromMaybe [] maybeFocusAction
      maybeFocusAction = (getCurrentSequent model >>= \f -> Just $ pathToLineNumber f path) >>= getFocusAction
      getFocusAction l = Just [ SetFocusOnKey (WidgetKey $ showt l <> ".statement") ]

  SwitchSubProofToLine path widgetKey -> applyOnCurrentProof model switch ++ [SetFocusOnKey widgetKey, MoveFocusFromKey Nothing FocusFwd]-- ++ focusAction
    where
      switch p = if not $ isSingleton $ evalPath p path then p else replaceInProof path (\oldLine -> case oldLine of
        SubProof p -> head p
        _ -> error ""
        ) p
      isSingleton (SubProof p) = length p == 1
      isSingleton _ = False

      focusAction = fromMaybe [] maybeFocusAction
      maybeFocusAction = (getCurrentSequent model >>= \f -> Just $ pathToLineNumber f path) >>= getFocusAction
      getFocusAction l = Just [ SetFocusOnKey (WidgetKey $ showt l <> ".statement"), MoveFocusFromKey Nothing FocusFwd, MoveFocusFromKey Nothing FocusFwd ]

  InsertLineAfter path -> applyOnCurrentProof model insertLine ++ focusAction
    where
      focusAction = fromMaybe [] maybeFocusAction
      maybeFocusAction = (getCurrentSequent model >>= \f -> Just $ pathToLineNumber f path) >>= getFocusAction
      getFocusAction l = Just [ SetFocusOnKey (WidgetKey $ showt (l + 1) <> ".statement") ]
      insertLine = insertAfterProof path (Line "" "")

  InsertSubProofAfter path -> applyOnCurrentProof model insertSubProof
    where insertSubProof = insertAfterProof path (SubProof [Line "" ""])

  AddLine -> applyOnCurrentProof model addLine
    where
      addLine sequent = FESequent premises conclusion steps
        where
          premises = _premises sequent
          conclusion = _conclusion sequent
          steps = _steps sequent ++ [Line "" ""]

  AddSubProof -> applyOnCurrentProof model addSubProof
    where
      addSubProof sequent = FESequent premises conclusion steps
        where
          premises = _premises sequent
          conclusion = _conclusion sequent
          steps = _steps sequent ++ [SubProof [Line "" ""]]

  RemoveLine path -> applyOnCurrentProof model removeLine ++ focusAction
    where
      removeLine = removeFromProof path

      focusAction = fromMaybe [] (getCurrentSequent model >>= Just . getFocusAction)
      getFocusAction sequent = [ SetFocusOnKey (WidgetKey $ showt l <> ".rule") ]
        where l = pathToLineNumber sequent path - 1

  EditLine path arg newText -> applyOnCurrentProof model editLine
    where editLine = editFormulaInProof path arg newText

  OpenCreateProofPopup -> [ Model $ model & newFilePopupOpen .~ True ]

  CreateEmptyProof input -> [
      Producer (\sendMsg -> do
        exists <- doesFileExist filePath
        if exists then return () else do
          writeFile filePath emptyProof
          sendMsg (OpenFile fileName)
          sendMsg RefreshExplorer
      ),
      Model $ model
        & newFilePopupOpen .~ False
        & newFileName .~ ""
    ]
    where
      filePath = model ^. workingDir </> fileName
      fileName = unpack (trimExtension ".logic" input) <> ".logic"
      emptyProof = ";;{ : ;}"

  RefreshExplorer -> [ Producer $ directoryFilesProducer (model ^. workingDir) ]

  SetFilesInDirectory fs -> [ Model $ model & filesInDirectory .~ fs ]

  OpenFile_ filePath folderPath -> [
      Producer (\sendMsg -> do
        let fullPath = folderPath </> filePath
        pContent <- readFile fullPath
        let pContentText = pack pContent

        if takeExtension fullPath == ".md" then
          sendMsg (OpenFileSuccess $ MarkdownFile fullPath pContentText)
        else if fullPath == "Settings.json" && folderPath == "" then
          sendMsg (OpenFileSuccess $ SettingsFile fullPath)
        else if takeExtension fullPath == ".logic" then
          do
            let pIsEdited = False

            case pSequent (myLexer pContent) of
              Left _err -> do
                let seq = parseProofFromSimpleFileFormat pContentText
                sendMsg (OpenFileSuccess $ ProofFile fullPath pContentText seq pIsEdited)
                -- sendMsg (OpenFileSuccess $ File fullPath pContentText Nothing pIsEdited)

              Right seq_t -> sendMsg (OpenFileSuccess $ ProofFile fullPath pContentText pParsedContent pIsEdited)
                where pParsedContent = Just (convertSeq seq_t)

            -- let pContentText = pack pContent
            --     pParsedContent = Just (parseProofFromSimpleFileFormat pContentText)
            --     pIsEdited = False
            -- sendMsg (OpenFileSuccess $ File fullPath pContentText pParsedContent pIsEdited)
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
            -- Abs.FormOr _ _ -> p
            Abs.FormImpl _ _ -> p
            _ -> c
            where c = convertForm form; p = "(" <> c <> ")"

      convertForm (Abs.FormOr a b) = getOutput a <> " | " <> getOutput b
        where
          getOutput form = case form of
            -- Abs.FormAnd _ _ -> p
            Abs.FormImpl _ _ -> p
            _ -> c
            where c = convertForm form; p = "(" <> c <> ")"

      convertForm (Abs.FormImpl a b) = convertForm a <> " -> " <> convertForm b
      convertForm (Abs.FormPred (Abs.Pred (Abs.Ident i) _params)) = pack i
      convertForm Abs.FormBot = "bot"
      convertForm (Abs.FormPar a) = "(" <> convertForm a <> ")"
      convertForm (Abs.FormEq _ _) = error "= not implemented"
      convertForm (Abs.FormAll _ _) = error "forall not implemented"
      convertForm (Abs.FormSome _ _) = error "exists not implemented"
      convertForm Abs.FormNil = error "FormNil not implemented"

      convertProof (Abs.Proof proofElems) = concat $ map convertProofElem proofElems
      convertProofElem (Abs.ProofElem _labels step) = convertStep step

      convertStep Abs.StepNil = [Line "" ""]
      convertStep (Abs.StepPrem _form) = [] -- [Line (convertForm form) "prem"]
      convertStep (Abs.StepAssume form) = [Line (convertForm form) "assume"]
      convertStep (Abs.StepProof proof) = [SubProof (convertProof proof)]
      convertStep (Abs.StepForm (Abs.Ident i) args form) = [Line (convertForm form) (pack i <> " [" <> intercalate ", " (map convertArg args) <> "]")]
      convertStep (Abs.StepDecConst _) = error "StepDecConst not implemented"
      convertStep (Abs.StepDecVar _) = error "StepDecVar not implemented"
      convertStep (Abs.StepDecFun _ _) = error "StepDecFun not implemented"

      convertArg (Abs.ArgLine i) = showt i
      convertArg (Abs.ArgRange a b) = showt a <> "-" <> showt b
      convertArg (Abs.ArgTerm (Abs.Term (Abs.Ident i) _)) = pack i

  OpenFile filePath -> handleEvent wenv node model (OpenFile_ filePath (model ^. workingDir))

  OpenFileSuccess file -> Model newModel : handleEvent wenv node newModel (SetCurrentFile filePath)
    where
      newModel = model
        & openFiles %~ doOpenFile
        & tmpLoadedFiles %~ createNew file

      doOpenFile currentlyOpenFiles = currentlyOpenFiles ++ [filePath | filePath `notElem` model ^. openFiles]
      createNew newFile oldFiles = if _path newFile `elem` (model ^. openFiles) then
          oldFiles else
          filter (\f -> _path newFile /= _path f) oldFiles ++ [newFile]
      filePath = _path file

  CloseCurrentFile -> case model ^. currentFile of
    Nothing -> []
    Just filePath -> handleEvent wenv node model (CloseFile filePath)

  CloseFile filePath -> case file of
    Just file@ProofFile {} -> if _isEdited file then [
        Model $ model
          & confirmDeletePopup .~ True
          & confirmDeleteTarget .~ Just filePath
      ] else handleEvent wenv node model (CloseFileSuccess filePath)
    Just _ -> handleEvent wenv node model (CloseFileSuccess filePath)
    Nothing -> []
    where file = getProofFileByPath (model ^. tmpLoadedFiles) filePath

  CloseFileSuccess filePath -> [ Model finalModel ]
    where
      finalModel = modelWithClosedFile
        & currentFile .~ (if cf == Just filePath then maybeHead (modelWithClosedFile ^. openFiles) else cf)
      modelWithClosedFile = model
        & openFiles %~ filter (filePath/=)
        & confirmDeleteTarget .~ Nothing
        & confirmDeletePopup .~ False
      cf = model ^. currentFile

  SaveCurrentFile -> case model ^. currentFile of
    Nothing -> []
    Just filePath -> case currentFile of
      Just file@ProofFile {} -> handleEvent wenv node model (SaveProof file)
      _ -> []
      where currentFile = getProofFileByPath (model ^. tmpLoadedFiles) filePath

  SaveProof f -> case _parsedSequent f of
    Nothing -> []
    Just seq -> [
        Producer (\sendMsg -> do
          let content = (unpack . parseProofToSimpleFileFormat) seq
          -- let content = (unpack . parseProofForBackend) seq
              fileName = _path f

          result <- try (writeFile (model ^. workingDir </> fileName) content) :: IO (Either SomeException ())
          case result of
            Left _ -> return ()
            Right _ -> sendMsg (SaveProofSuccess f)
        )
      ]

  SaveProofSuccess f -> actions
    where
      actions = fromMaybe [] (fileIndex >>= Just . getActions)
      getActions fileIndex = [ Model $ model & tmpLoadedFiles . singular (ix fileIndex) . isEdited .~ False ]
      fileIndex = getProofFileIndexByPath (model ^. tmpLoadedFiles) (_path f)

  SetCurrentFile filePath -> [
      Model $ model
        & currentFile ?~ filePath
        & proofStatus .~ Nothing
    ]

  SwitchTheme -> [
      Model $ model & selectedTheme %~ switchTheme
    ]
    where
      switchTheme oldTheme
        | oldTheme == customLightTheme = customDarkTheme
        | oldTheme == customDarkTheme = customLightTheme
        | otherwise = customLightTheme

  UpdateFont s -> [Model $ model & normalFont .~ head s]

  ReadSettings -> [
      Producer (\sendMsg -> do
        pContent <- readFile "Settings.json"
        sendMsg (ReadSettings_ pContent)
      )
    ]
  ReadSettings_ settings -> [Model $ model & testSetting .~ pack settings]

  -- Backend events
  CheckProof file -> [
      Model $ model & proofStatus .~ Nothing,
      Producer (evaluateCurrentProof model file)
    ]

  BackendResponse (StringSequentChecked result) -> case result of
    Left error -> [ Model $ model & proofStatus ?~ Left error ]
    Right _ -> [ Model $ model & proofStatus ?~ Right () ]

  BackendResponse (SequentChecked result) -> case result of
    Left error -> [ Model $ model & proofStatus ?~ Left error ]
    Right _ -> [ Model $ model & proofStatus ?~ Right () ]

  BackendResponse (OtherBackendMessage message) -> [ Producer (\_ -> print $ "From backend: " ++ message) ]

  -- BackendResponse (SequentChecked result) -> case result of
  --   Left err -> [Message "Error" (pack err)]  -- Add type annotation
  --   Right sequent -> [Model $ model & proofStatus ?~ isProofCorrect sequent]

  -- BackendResponse (StepChecked result) -> case result of
  --   Left err -> [Message "Error" (pack err)]  -- Add type annotation
  --   Right _step -> [Message "Step Status" ("Step is correct" :: Text)]  -- Add type annotation

  OpenSetWorkingDir -> [ Producer openDiag ]
    where
      openDiag sendMsg = do
        path <- openFolderDialog
        case path of
          Nothing -> return ()
          Just path -> sendMsg (SetWorkingDir path)

  SetWorkingDir path -> [
      Model $ model & workingDir .~ path,
      Producer (directoryFilesProducer path)
    ]

  Print s -> [ Producer (\_ -> print s) ]

  -- Log unhandled events instead of crashing
  f -> [ Producer (\_ -> print f) ]



directoryFilesProducer :: FilePath -> (AppEvent -> IO ()) -> IO ()
directoryFilesProducer workingDir sendMsg = do
  allFileNames <- fmap (map (drop (length workingDir + 1))) (listDirectoryRecursive workingDir)
  sendMsg (SetFilesInDirectory allFileNames)

applyOnCurrentProof :: AppModel -> (FESequent -> FESequent) -> [EventResponse AppModel e sp ep]
applyOnCurrentProof model f = actions
  where
    actions = fromMaybe [] (fileIndex >>= Just . getActions)
    fileIndex = cf >>= getProofFileIndexByPath (model ^. tmpLoadedFiles)
    cf = model ^. currentFile

    getActions fileIndex = [
        Model $ model
          & tmpLoadedFiles . singular (ix fileIndex) . parsedSequent %~ maybeF
          & tmpLoadedFiles . singular (ix fileIndex) . isEdited .~ True
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

editFormulaInProof :: FormulaPath -> Int -> Text -> FESequent -> FESequent
editFormulaInProof path arg newText = replaceSteps f
  where
    f steps = zipWith (\p idx -> el path arg newText [idx] p) steps [0..]
    el editPath arg newText currentPath (SubProof p) = SubProof $ zipWith (\p idx -> el editPath arg newText (currentPath ++ [idx]) p) p [0..]
    el editPath arg newText currentPath f@(Line statement rule)
      | editPath == currentPath = case arg of
        0 -> Line newText rule
        1 -> Line statement newText
        _ -> error "Invalid field, should be 0 or 1"
      | otherwise = f

replaceInProof :: FormulaPath -> (FEStep -> FEStep) -> FESequent -> FESequent
replaceInProof path replaceWith = replaceSteps f
  where
    f steps = zipWith (\p idx -> rl [idx] p) steps [0..]
    rl currentPath f@(SubProof p)
      | path == currentPath = replaceWith f
      | otherwise = SubProof $ zipWith (\p idx -> rl (currentPath ++ [idx]) p) p [0..]
    rl currentPath f@(Line _ _)
      | path == currentPath = replaceWith f
      | otherwise = f

-- insertBeforeProof :: FormulaPath -> FEStep -> FESequent -> FESequent
-- insertBeforeProof path insertThis = replaceSteps f
--   where
--     f steps = concat (zipWith (\p idx -> rl [idx] p) steps [0..])

--     rl currentPath (SubProof p)
--       | path == currentPath = [res, insertThis]
--       | otherwise = [res]
--         where res = SubProof $ concat $ zipWith (\p idx -> rl (currentPath ++ [idx]) p) p [0..]
--     rl currentPath f@(Line _ _)
--       | path == currentPath = [f, insertThis]
--       | otherwise = [f]

insertAfterProof :: FormulaPath -> FEStep -> FESequent -> FESequent
insertAfterProof path insertThis = replaceSteps f
  where
    f steps = concat (zipWith (\p idx -> rl [idx] p) steps [0..])

    rl currentPath (SubProof p)
      | path == currentPath = [res, insertThis]
      | otherwise = [res]
        where res = SubProof $ concat $ zipWith (\p idx -> rl (currentPath ++ [idx]) p) p [0..]
    rl currentPath f@(Line _ _)
      | path == currentPath = [f, insertThis]
      | otherwise = [f]

removeFromProof :: FormulaPath -> FESequent -> FESequent
removeFromProof path = replaceSteps f
  where
    f steps = if null res then startProof else res
      where
        startProof = [Line "" ""]
        res = filterValid $ zipWith (\p idx -> rl path [idx] p) steps [0..]

    rl removePath currentPath (SubProof p)
      | removePath == currentPath = Nothing
      | otherwise = Just $ SubProof $ filterValid $ zipWith (\p idx -> rl removePath (currentPath ++ [idx]) p) p [0..]
    rl removePath currentPath f@(Line _ _)
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
    fileIndex = cf >>= getProofFileIndexByPath (model ^. tmpLoadedFiles)
    cf = model ^. currentFile

    getSequent fileIndex = case model ^. tmpLoadedFiles . singular (ix fileIndex) of
      ProofFile _path _content _parsedSequent _isEdited -> _parsedSequent
      _ -> Nothing


    -- getSequent fileIndex = model ^. tmpLoadedFiles . singular (ix fileIndex) . parsedSequent

getProofFileIndexByPath :: [File] -> FilePath -> Maybe Int
getProofFileIndexByPath allFiles filePath = findIndex (\f -> _path f == filePath) allFiles

evaluateCurrentProof :: AppModel -> File -> (AppEvent -> IO ()) -> IO ()
evaluateCurrentProof model file sendMsg = do
  -- let sequent = exportProof file
  -- answer <- evaluateProofSegment (model ^. frontendChan) (model ^. backendChan) sequent
  -- sendMsg (BackendResponse answer)

  -- catch (putStrLn $ unpack $ parseProofForBackend (_parsedSequent file)) (print :: ErrorCall -> IO ())
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