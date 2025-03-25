{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.HandleEvent (
  handleEvent
) where

import Frontend.Types
import Frontend.SpecialCharacters
import Frontend.Helper
import Frontend.Themes
import Frontend.Communication (startCommunication, evaluateProofString)
import Shared.Messages
import qualified Logic.Abs as Abs
import Logic.Par (pSequent, myLexer)

import Monomer
import Control.Lens
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay, newChan)
import Control.Monad (filterM)
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (findIndex)
import Data.Text (Text, unpack, pack, intercalate)
import TextShow ( TextShow(showt) )
import System.Directory ( doesFileExist, listDirectory, doesDirectoryExist )

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  NoEvent -> []

  AppInit -> [
      Producer directoryFilesProducer,
      Task $ do
        frontendChan <- newChan
        backendChan <- newChan
        answer <- startCommunication frontendChan backendChan
        return $ BackendResponse answer
    ]

  SetOpenMenuBarItem s -> [ Model $ model & openMenuBarItem .~ s ]

  FocusOnKey key -> [ SetFocusOnKey key ]

  NextFocus n -> replicate n (MoveFocusFromKey Nothing FocusFwd)

  AddPremise -> applyOnCurrentProof model addPremiseToProof

  RemovePremise idx -> applyOnCurrentProof model (removePremiseFromProof idx)

  EditPremise idx newText -> applyOnCurrentProof model (editPremisesInProof idx newText)

  EditConclusion newText -> applyOnCurrentProof model (editConclusionInProof newText)

  SwitchLineToSubProof path -> applyOnCurrentProof model switch ++ focusAction
    where
      switch = replaceInProof path (\oldLine -> SubProof [oldLine])

      focusAction = fromMaybe [] maybeFocusAction
      maybeFocusAction = (getCurrentSequent model >>= \f -> Just $ pathToLineNumber f path) >>= getFocusAction
      getFocusAction l = Just [ SetFocusOnKey (WidgetKey $ showt l <> ".statement"), MoveFocusFromKey Nothing FocusBwd ]

  SwitchSubProofToLine path -> applyOnCurrentProof model switch ++ focusAction
    where
      switch p = if not $ isSingleton $ evalPath p path then p else replaceInProof path (\oldLine -> case oldLine of
        SubProof p -> head p
        _ -> error ""
        ) p
      isSingleton (SubProof p) = length p == 1
      isSingleton _ = False

      focusAction = fromMaybe [] maybeFocusAction
      maybeFocusAction = (getCurrentSequent model >>= \f -> Just $ pathToLineNumber f path) >>= getFocusAction
      getFocusAction l = Just [ SetFocusOnKey (WidgetKey $ showt l <> ".statement"), MoveFocusFromKey Nothing FocusFwd ]

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

  RemoveLine path -> applyOnCurrentProof model removeLine
    where removeLine = removeFromProof path

  EditLine path arg newText -> applyOnCurrentProof model editLine
    where editLine = editFormulaInProof path arg newText

  OpenCreateProofPopup -> [ Model $ model & newFilePopupOpen .~ True ]

  CreateEmptyProof fileName -> [
      Producer (\sendMsg -> do
        exists <- doesFileExist filePath
        if exists then return () else do
          writeFile filePath emptyProof
          sendMsg (OpenFile shortFilePath)
      ),
      Model $ model
        & newFilePopupOpen .~ False
        & newFileName .~ ""
    ]
    where
      filePath = "./myProofs/" <> shortFilePath
      shortFilePath = unpack fileName <> ".logic"
      emptyProof = "|-P{}"

  SetFilesInDirectory fs -> [ Model $ model & filesInDirectory .~ fs ]

  OpenFile_ filePath folderPath -> [
      Producer (\sendMsg -> do
        pContent <- readFile (folderPath <> filePath)
        let pContentText = pack pContent
            -- pParsedContent = parseProofFromFile pContentText
            pIsEdited = False
        case pSequent (myLexer pContent) of
          Left _err -> sendMsg (OpenFileSuccess $ File filePath pContentText Nothing pIsEdited)
          Right seq_t -> sendMsg (OpenFileSuccess $ File filePath pContentText pParsedContent pIsEdited)
            where pParsedContent = Just (convertSeq seq_t)
      )
    ]
    where
      convertSeq (Abs.Seq premises conclusion proof) = FESequent (map convertForm premises) (convertForm conclusion) (convertProof proof)

      -- convertForm (Abs.FormEq a b) = (convertForm a) <> " = " <> (convertForm b)
      -- convertForm (Abs.FormAll a b) = "#"
      -- convertForm (Abs.FormSome a b) = "#"
      convertForm (Abs.FormNot a) = "!" <> getOutput a
        where
          getOutput form = case form of
            Abs.FormPred _ -> c
            Abs.FormNot _ -> c
            Abs.FormBot -> c
            _ -> p
            where c = convertForm form; p = "(" <> c <> ")"

      convertForm (Abs.FormAnd a b) = getOutput a <> " & " <> getOutput b
        where
          getOutput form = case form of
            Abs.FormOr _ _ -> p
            Abs.FormIf _ _ -> p
            _ -> c
            where c = convertForm form; p = "(" <> c <> ")"

      convertForm (Abs.FormOr a b) = getOutput a <> " | " <> getOutput b
        where
          getOutput form = case form of
            Abs.FormAnd _ _ -> p
            Abs.FormIf _ _ -> p
            _ -> c
            where c = convertForm form; p = "(" <> c <> ")"

      convertForm (Abs.FormIf a b) = convertForm a <> " -> " <> convertForm b
      convertForm (Abs.FormPred (Abs.Pred (Abs.Ident i) _params)) = pack i
      convertForm Abs.FormBot = "bot"
      convertForm s = error (show s)

      convertProof (Abs.Proof proofElems) = concat $ map convertProofElem proofElems
      convertProofElem (Abs.ProofElem _labels step) = convertStep step

      convertStep (Abs.StepPrem _form) = [] -- [Line (convertForm form) "prem"]
      convertStep (Abs.StepAssume form) = [Line (convertForm form) "assume"]
      convertStep (Abs.StepProof proof) = [SubProof (convertProof proof)]
      convertStep (Abs.StepForm (Abs.Ident i) args form) = [Line (convertForm form) (pack i <> " [" <> intercalate ", " (map convertArg args) <> "]")]
      convertStep s = error (show s)

      convertArg (Abs.ArgLine i) = showt i
      convertArg (Abs.ArgRange a b) = showt a <> "-" <> showt b
      convertArg (Abs.ArgTerm (Abs.Term (Abs.Ident i) _)) = pack i
  
  OpenFile filePath -> handleEvent wenv node model (OpenFile_ filePath "./myProofs/")

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
    Just file -> if _isEdited file then [
        Model $ model
          & confirmDeletePopup .~ True
          & confirmDeleteTarget .~ Just filePath
      ] else handleEvent wenv node model (CloseFileSuccess filePath)
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

  SaveProof f -> case _parsedSequent f of
    Nothing -> []
    Just seq -> [
        Producer (\sendMsg -> do
          let content = (unpack . parseProofForBackend) seq
            -- content = unpack $ parseProofToFile $ _parsedSequent f
              fileName = _path f

          result <- try (writeFile ("./myProofs/" <> fileName) content) :: IO (Either SomeException ())
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

  -- Log unhandled events instead of crashing
  f -> [ Producer (\_ -> print f) ]



directoryFilesProducer :: (AppEvent -> IO ()) -> IO ()
directoryFilesProducer sendMsg = do
  let dir = "./myProofs"
  allFileNames <- fmap (map (drop (length dir + 1))) (listDirectoryRecursive dir)
  -- onlyFiles <- filterM (doesFileExist . (dir++)) allFileNames
  -- onlyDirs <- filterM (doesDirectoryExist . (dir++)) allFileNames
  sendMsg (SetFilesInDirectory allFileNames)

  threadDelay $ 2 * seconds
  directoryFilesProducer sendMsg
    where seconds = 1000 * 1000

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

    getSequent fileIndex = model ^. tmpLoadedFiles . singular (ix fileIndex) . parsedSequent

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

parseProofForBackend :: FESequent -> Text
parseProofForBackend sequent = premises <> " |- " <> conclusion <> " " <> exportProofHelper 0 [] proof
  where
    premises = replaceSpecialSymbolsInverse $ intercalate "," (_premises sequent)
    conclusion = replaceSpecialSymbolsInverse $ _conclusion sequent

    newSequent = FESequent (_premises sequent) (_conclusion sequent) (ghostPremises ++ _steps sequent)
    proof = SubProof (_steps newSequent)
    ghostPremises = map (\p -> Line p "prem") (_premises sequent)

    exportProofHelper :: Int -> FormulaPath -> FEStep -> Text
    exportProofHelper indent path (SubProof p) = tabs indent <> label <> "{\n" <> intercalate "\n" (zipWith (\p idx -> exportProofHelper (indent + 1) (path ++ [idx]) p) p [0..]) <> "\n" <> tabs indent <> "}"
      where label = if null p || null path then "" else showt (offsetLineNumber (path ++ [0])) <> "-" <> showt (offsetLineNumber (path ++ [length p - 1])) <> ":"
    exportProofHelper indent path (Line statement rule) = tabs indent <> label <> nRule <> " " <> nStatement <> ";"
      where
        nRule = replaceSpecialSymbolsInverse rule
        nStatement = replaceSpecialSymbolsInverse statement
        label = showt (offsetLineNumber path) <> ":"

    offsetLineNumber path = pathToLineNumber newSequent path-- + toInteger (length (_premises sequent))

    tabs :: Int -> Text
    tabs n = pack $ replicate n '\t'

pathToLineNumber :: FESequent -> FormulaPath -> Integer
pathToLineNumber sequent path = ep path (SubProof $ _steps sequent) 1
  where
    ep (idx:tail) currentProof startLine = case currentProof of
      SubProof p -> ep tail (p !! idx) (startLine + sum (map stepLength (take idx p)))
      Line _ _ -> error "Tried to index into `Line` (not an array)"
    ep [] _ startLine = startLine

    stepLength (SubProof p) = sum $ map stepLength p
    stepLength (Line _ _) = 1

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