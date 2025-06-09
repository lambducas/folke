{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.Helper.ProofHelper where

import Frontend.Types
import Frontend.Helper.General
import Frontend.Parse
import Frontend.Communication
import qualified Logic.Abs as Abs
import Logic.Par (pArg, myLexer)

import Monomer
import Control.Lens
import TextShow (showt)
import Data.Text (Text, unpack, pack)
import Data.List (findIndex)
import qualified Data.List
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map
import System.FilePath (equalFilePath)
import Shared.SpecialCharacters (replaceSpecialSymbols)

{-|
Applies a function on the currently opened file-tab
-}
applyOnCurrentFile :: AppModel -> (File -> File) -> [EventResponse AppModel AppEvent sp ep]
applyOnCurrentFile model f = actions
  where
    actions = fromMaybe [] (fileIndex >>= Just . getActions)
    fileIndex = cf >>= getProofFileIndexByPath (model ^. persistentState . tmpLoadedFiles)
    cf = model ^. persistentState . currentFile
    getActions fileIndex = [
        Model $ model
          & persistentState . tmpLoadedFiles . singular (ix fileIndex) %~ f,
          -- & proofStatus .~ Nothing,

        Event AutoCheckProof
      ]

{-|
Applies a function on the proof of the currently opened file-tab.
The function assumes that the current file is a proof-file and will
fail otherwise
-}
applyOnCurrentProof :: AppModel -> (FESequent -> FESequent) -> [EventResponse AppModel AppEvent sp ep]
applyOnCurrentProof model f = actions
  where
    actions = applyOnCurrentFile model updateSequent
    updateSequent file =
      file
        & parsedDocument %~ maybeF
        & isEdited .~ True
    maybeF (Just d) = Just $ d & sequent %~ f
    maybeF Nothing = Nothing

applyOnCurrentFEDocument :: AppModel -> (FEDocument -> FEDocument) -> [EventResponse AppModel AppEvent sp ep]
applyOnCurrentFEDocument model f = actions
  where
    actions = applyOnCurrentFile model updateSequent
    updateSequent file =
      file
        & parsedDocument %~ maybeF
        & isEdited .~ True
    maybeF (Just d) = Just $ f d
    maybeF Nothing = Nothing

{-|
Applies a function on the proof of the currently opened file-tab.
The function should return the new sequent and an event describing the change.
The function assumes that the current file is a proof-file and will
fail otherwise.
-}
applyOnCurrentProofAndRecordHistory :: AppModel -> (FESequent -> (FESequent, HistoryEvent)) -> [EventResponse AppModel AppEvent sp ep]
applyOnCurrentProofAndRecordHistory model f = applyOnCurrentFile model updateFile
  where
    updateFile file = case _parsedDocument file of
      Nothing -> file
      Just doc -> file
        & isEdited .~ True
        & parsedDocument  . _Just . sequent .~ fst fApplied
        & history . hIndex %~ (+1)
        & history . hState %~ forkHistory (_hIndex (_history file))
        where
          forkHistory index state = take (index + 1) state ++ [snd fApplied]
          fApplied = f $ _sequent doc

applyOnCurrentFEDocumentAndRecordHistory :: AppModel -> (FEDocument -> (FEDocument, HistoryEvent)) -> [EventResponse AppModel AppEvent sp ep]
applyOnCurrentFEDocumentAndRecordHistory model f = applyOnCurrentFile model updateFile
  where
    updateFile file = case _parsedDocument file of
      Nothing -> file
      Just doc -> file
        & isEdited .~ True
        & parsedDocument . _Just .~ fst fApplied
        & history . hIndex %~ (+1)
        & history . hState %~ forkHistory (_hIndex (_history file))
        where
          forkHistory index state = take (index + 1) state ++ [snd fApplied]
          fApplied = f doc

{-|
Applies a function on the proof of the currently opened file-tab and
records a general sequent-change event.
The function assumes that the current file is a proof-file and will
fail otherwise.
-}
applyOnCurrentProofAndRecordSequentHistory :: AppModel -> (FESequent -> FESequent) -> [EventResponse AppModel AppEvent sp ep]
applyOnCurrentProofAndRecordSequentHistory model f = applyOnCurrentProofAndRecordHistory model newF
  where
    newF seq = (newSeq, HUpdateSequent seq newSeq)
      where newSeq = f seq

-- | Insert premise at index and updates all line references
addPremiseToProof :: Int -> Text -> FESequent -> FESequent
addPremiseToProof idx premise sequent = FESequent premises conclusion (steps' sequent)
  where
    premises = insertAt premise idx (_premises sequent)
    conclusion = _conclusion sequent
    steps' seq = _steps (offsetAllRefs [] 1 (toInteger idx+1) False seq)

-- | Removes the n:th premise in proof and updates all line references
removePremiseFromProof :: Int -> FESequent -> FESequent
removePremiseFromProof idx sequent = FESequent premises conclusion (steps' sequent)
  where
    premises = _premises sequent ^.. folded . ifiltered (\i _ -> i /= idx)
    conclusion = _conclusion sequent
    steps' seq = _steps (offsetAllRefs [] (-1) (toInteger idx+1) True seq)

-- | Updates the n:th premise in proof
editPremisesInProof :: Int -> Text -> FESequent -> FESequent
editPremisesInProof idx newText sequent = FESequent { _premises = premises, _conclusion = conclusion, _steps = steps }
  where
    premises = _premises sequent & element idx .~ replaceSpecialSymbols newText
    conclusion = _conclusion sequent
    steps = _steps sequent

-- | Updates the conclusion in proof
editConclusionInProof :: Text -> FESequent -> FESequent
editConclusionInProof newText sequent = FESequent { _premises = premises, _conclusion = conclusion, _steps = steps }
  where
    premises = _premises sequent
    conclusion = replaceSpecialSymbols newText
    steps = _steps sequent

-- | Updates the formula on line at given path
editFormulaInProof :: AppModel -> FormulaPath -> Text -> FEDocument -> FEDocument
editFormulaInProof model path newText doc = doc & sequent .~ replaceSteps f seq
  where
    seq = _sequent doc
    oldText = case evalPath path seq of
      SubProof _ -> ""
      Line statement _ _ _  -> statement
    f steps = zipWith (\p idx -> el path newText [idx] p) steps [0..]
    el editPath newText currentPath (SubProof p) = SubProof $ zipWith (\p idx -> el editPath newText (currentPath ++ [idx]) p) p [0..]
    el editPath newText currentPath f@(Line _statement rule usedArguments arguments)
      | editPath == currentPath = Line { _statement = parseFormulaInput model oldText newText, _rule = rule, _usedArguments = usedArguments, _arguments = arguments }
      | otherwise = f

-- | Replace special characters and also A, E if setting is enabled (always right now...)
parseFormulaInput :: AppModel -> Text -> Text -> Text
parseFormulaInput model oldText newText
  | model ^. preferences . replaceAEInFormula = replaceSpecialSymbols . doReplaceText $ newText
  | otherwise = replaceSpecialSymbols newText
  where
    -- Guess where insertion was made
    afterIdx = firstMissmatch oldText newText :: Integer

    doReplaceText = pack . doReplace 0 . unpack
    doReplace _ [] = ""
    doReplace idx (x:xs)
      | idx == afterIdx = replaceSingleChar x : doReplace (idx + 1) xs
      | otherwise = x : doReplace (idx + 1) xs

    replaceSingleChar 'A' = '∀'
    replaceSingleChar 'E' = '∃'
    replaceSingleChar 'v' = '∨'
    replaceSingleChar 'a' = '∧'
    replaceSingleChar '-' = '¬'
    replaceSingleChar t = t

firstMissmatch :: (Num t) => Text -> Text -> t
firstMissmatch a b = tw (unpack a) (unpack b) 0
  where
    tw (x:xs) (y:ys) i
      | x == y = tw xs ys (i+1)
      | otherwise = i
    tw [] [] i = i
    tw [] _ i = i
    tw _ [] i = i

-- | Updates the rule on line at given path
editRuleNameInProof :: FormulaPath -> Text -> FEDocument -> FEDocument
editRuleNameInProof path newRule doc = doc & sequent .~ replaceSteps f (_sequent doc)
  where
    f steps = zipWith (\p idx -> el path [idx] p) steps [0..]
    el editPath currentPath (SubProof p) = SubProof $ zipWith (\p idx -> el editPath (currentPath ++ [idx]) p) p [0..]
    el editPath currentPath f@(Line statement _rule usedArguments arguments)
      | editPath == currentPath = case Data.Map.lookup newRuleParsed ruleMetaDataMap of
        Nothing -> case getUDR newRuleParsed of
          Nothing -> Line statement (replaceSpecialSymbols newRule) usedArguments arguments
          Just udr -> Line statement (replaceSpecialSymbols newRule) nrArgs (fillList (fromIntegral nrArgs) arguments)
            where nrArgs = maybe 0 length (_udrInput udr)
        Just (RuleMetaData {_nrArguments=nrArgs}) -> Line statement (replaceSpecialSymbols newRule) (fromIntegral nrArgs) (fillList nrArgs arguments)
      | otherwise = f

    fillList :: Integer -> [Text] -> [Text]
    fillList targetLen arr
      | currLen < targetLen = arr ++ replicate (fromIntegral targetLen - length arr) ""
      | otherwise = arr
      where currLen = toInteger (length arr)

    getUDR name = case _fedUserDefinedRules doc of
      Nothing -> Nothing
      Just rules -> Data.List.find (\r -> _udrName r == name) rules

    newRuleParsed = parseRule newRule

-- | Updates the n:th argument on line at given path with new text
editRuleArgumentInProof :: FormulaPath -> Int -> Text -> FEDocument -> FEDocument
editRuleArgumentInProof path idx newText doc = doc & sequent .~ replaceSteps f (_sequent doc)
  where
    f steps = zipWith (\p idx -> el path [idx] p) steps [0..]
    el editPath currentPath (SubProof p) = SubProof $ zipWith (\p idx -> el editPath (currentPath ++ [idx]) p) p [0..]
    el editPath currentPath f@(Line statement rule usedArguments arguments)
      -- Only =E requires phi function
      | editPath == currentPath && rule == "=E"
        = Line {
          _statement = statement,
          _rule = rule,
          _usedArguments = usedArguments,
          _arguments = arguments & element idx .~ ("u:=" <> replaceSpecialSymbols newText)
        }
      | editPath == currentPath
        = Line {
          _statement = statement,
          _rule = rule,
          _usedArguments = usedArguments,
          _arguments = arguments & element idx .~ replaceSpecialSymbols newText
        }
      | otherwise = f

-- | Apply function to sequent and record history
editLineAndRecordHistory :: AppModel -> FormulaPath -> (FEDocument -> FEDocument) -> [EventResponse AppModel AppEvent sp ep]
editLineAndRecordHistory model path f = applyOnCurrentFEDocumentAndRecordHistory model helperF
  where
    helperF doc = (newDoc, HEditStep path oldStep newStep)
      where
        oldStep = evalPath path (_sequent doc)
        newStep = evalPath path (_sequent newDoc)
        newDoc = f doc

-- | Applies a given function to the step at path
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

-- | Inserts a given step into proof such that it has the given path
insertInProof :: FormulaPath -> FEStep -> FESequent -> FESequent
insertInProof [] _ seq = seq
insertInProof [n] insertThis seq = replaceSteps f seq
  where f = insertAt insertThis n
insertInProof path insertThis seq = replaceInProof parentPath g fixedSeq
  where
    fixedSeq = seq --createParentsIfNeeded parentPath seq

    g (SubProof steps) = SubProof $ insertAt insertThis (last path) steps
    g other = other
    parentPath = init path

    -- createParentsIfNeeded parentPath seq = case evalPathSafe parentPath seq of
    --   Nothing -> insertInProof parentPath (SubProof []) seq
    --   Just (Line {}) -> insertInProof parentPath (SubProof []) seq
    --   Just _ -> seq

-- | Same as `insertAfterProof` but can optionally update all line references
insertAfterAndMaybeUpdateRefs :: FormulaPath -> FEStep -> Bool -> FESequent -> FESequent
insertAfterAndMaybeUpdateRefs path insertThis updateRef = insertAfterProof path insertThis . offsetFunc
  where
    offsetFunc seq = if updateRef then offsetAllRefs path 1 lineNumber False seq else seq
      where
        d = evalPath path seq
        isSubProof (SubProof {}) = True
        isSubProof _ = False
        lineNumber
          | isSubProof d = pathToLineNumberOffsetPremises seq path + proofStepLength d
          | otherwise = pathToLineNumberOffsetPremises seq path + 1

-- | Same as `insertBeforeProof` but can optionally update all line references
insertBeforeAndMaybeUpdateRefs :: FormulaPath -> FEStep -> Bool -> FESequent -> FESequent
insertBeforeAndMaybeUpdateRefs path insertThis updateRef = insertBeforeProof path insertThis . offsetFunc
  where
    offsetFunc seq = if updateRef then offsetAllRefs path 1 lineNumber False seq else seq
      where lineNumber = pathToLineNumberOffsetPremises seq path

-- | Inserts a line/subproof below a given path
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

-- | Inserts a line/subproof above a given path
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

{-|
Remove step at given path and optionally removes empty subproofs
which can result from removing the last line in a singleton subproof
-}
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

-- {-|
-- Removes empty subproofs and keeps a history of whats removed
-- which can be used to handle undo/redo
-- -}
-- removeInvalidFromProofWithHistory :: FESequent -> (FESequent, [HistoryEvent])
-- removeInvalidFromProofWithHistory seq = (seq & steps .~ fst res, snd res)
--   where
--     res = (filter validateProof children, events)
--       where
--         res = zipWith (\p idx -> rl [idx] p) (_steps seq) [0..]
--         children = map fst res
--         oldEvents = concatMap snd res
--         events = oldEvents ++ catMaybes (zipWith (\c i -> if validateProof c then Nothing else Just $ HRemoveStep False [i] c) children [0..])
--     rl currentPath (SubProof p) = (SubProof $ filter validateProof children, events)
--       where
--         children = map fst $ zipWith (\p idx -> rl (currentPath ++ [idx]) p) p [0..]
--         oldEvents = concatMap snd $ zipWith (\p idx -> rl (currentPath ++ [idx]) p) p [0..]
--         events = oldEvents ++ catMaybes (zipWith (\c i -> if validateProof c then Nothing else Just $ HRemoveStep False (currentPath ++ [i]) c) children [0..])
--     rl _currentPath f@(Line {}) = (f, [])

--     validateProof (SubProof []) = False
--     validateProof _ = True

-- | Removes empty subproofs from sequent
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

-- | Moves step at target-path to below destination-path
moveInProof :: FormulaPath -> FormulaPath -> Bool -> FESequent -> FESequent
moveInProof target dest removeInvalid seq
  | target > dest = (maybeRemoveInvalid . copyTarget seq . deleteTarget) seq
  | otherwise     = (maybeRemoveInvalid . deleteTarget . copyTarget seq) seq
  where
    maybeRemoveInvalid = if removeInvalid then removeInvalidFromProof else id
    copyTarget oldestSeq = insertInProof dest (evalPath target oldestSeq)
    -- copyTarget oldestSeq = insertAfterProof dest (evalPath target oldestSeq)
    deleteTarget = removeFromProof target False

-- | Helper to apply a function on the steps of a sequent
replaceSteps :: ([FEStep] -> [FEStep]) -> FESequent -> FESequent
replaceSteps f sequent = FESequent premises conclusion steps
  where
    premises = _premises sequent
    conclusion = _conclusion sequent
    steps = f $ _steps sequent

getCurrentFile :: AppModel -> Maybe File
getCurrentFile model = file
  where
    file = currentPath >>= getProofFileByPath (model ^. persistentState . tmpLoadedFiles)
    currentPath = model ^. persistentState . currentFile

{-|
Extracts the FEDocument from the currently opened tab.
Will return `Nothing` when no file is opened or the
opened filed isn't a proof
-}
getCurrentFEDocument :: AppModel -> Maybe FEDocument
getCurrentFEDocument model = document
  where
    document = fileIndex >>= getDocument
    fileIndex = cf >>= getProofFileIndexByPath (model ^. persistentState . tmpLoadedFiles)
    cf = model ^. persistentState . currentFile

    getDocument fileIndex = case model ^. persistentState . tmpLoadedFiles . singular (ix fileIndex) of
      f@ProofFile {} -> _parsedDocument f
      f@TemporaryProofFile {} -> _parsedDocument f
      _ -> Nothing

{-|
Extracts the sequent from the currently opened tab.
Will return `Nothing` when no file is opened or the
opened filed isn't a proof
-}
getCurrentSequent :: AppModel -> Maybe FESequent
getCurrentSequent model = sequent
  where
    sequent = (fileIndex >>= getDocument) >>= Just . _sequent
    fileIndex = cf >>= getProofFileIndexByPath (model ^. persistentState . tmpLoadedFiles)
    cf = model ^. persistentState . currentFile

    getDocument fileIndex = case model ^. persistentState . tmpLoadedFiles . singular (ix fileIndex) of
      f@ProofFile {} -> _parsedDocument f
      f@TemporaryProofFile {} -> _parsedDocument f
      _ -> Nothing

-- | Generates a list of events based on the current sequent
getEventUsingCurrentSequent :: AppModel -> (FESequent -> [a]) -> [a]
getEventUsingCurrentSequent model f = focusAction
  where
    focusAction = fromMaybe [] maybeFocusAction
    maybeFocusAction = getCurrentSequent model >>= Just . f

-- | Gets the index to a `File` by it's (hopefully) unique `FilePath` in a given list of files
getProofFileIndexByPath :: [File] -> FilePath -> Maybe Int
getProofFileIndexByPath allFiles filePath = findIndex (\f -> _path f `equalFilePath` filePath) allFiles

{-|
Sends file to proof checker to be validated. The response from the backend is
sent as a `BackendResponse` event back to the frontend
-}
evaluateCurrentProof :: AppModel -> File -> Bool -> Int -> (AppEvent -> IO ()) -> IO ()
evaluateCurrentProof model file acpFlag wrngSensetivity sendMsg = do
  case _parsedDocument file of
    Nothing -> return ()
    Just doc -> do
      -- let text = unpack $ parseProofForBackend seq
      -- putStrLn text

      answer <- evaluateProofFE (model ^. frontendChan) (model ^. backendChan) doc acpFlag wrngSensetivity
      sendMsg (BackendResponse answer)

{-|
Adds an offset to every rule argument which is referencing a
line or subproof. The offset is only applied to references
to lines with linenumbers greater than or equal to a given value.
Optionally invalidate line numbers equal to threshold (Used when
removing line)
-}
offsetAllRefs :: FormulaPath -> Integer -> Integer -> Bool -> FESequent -> FESequent
offsetAllRefs path by after invalidateEqual seq = applyOnAllRefs f seq
  where f = offsetLineRefBy path by after invalidateEqual seq

{-|
Adds an offset to a ***single*** rule argument which is referencing a
line or subproof. The offset is only applied to references
to lines with linenumbers greater than or equal to a given value.
Optionally invalidate line numbers equal to threshold (Used when
removing line)
-}
offsetLineRefBy :: FormulaPath -> Integer -> Integer -> Bool -> FESequent -> Text -> Text
offsetLineRefBy path by after invalidateEqual seq = applyOnLineNumberRef f
  where
    f arg whichArg l
      | isMatchingRangeRef r arg && fst r == snd r && invalidateEqual = "_"
      | whichArg == 0 && isMatchingRangeRef r arg = showt l
      | whichArg == 1 && isMatchingRangeRef r arg = showt $ l + by
      | l == after && invalidateEqual = "_"
      | l >= after = showt $ l + by
      | otherwise = showt l

    r = if null path then (-1, -1) else lineNumberRange (init path) seq

    isMatchingRangeRef (pa, pb) (Abs.ArgRange ra rb) = pa == ra && pb == rb
    isMatchingRangeRef _ _ = False

-- | Applies function to every rule argument in sequent
applyOnAllRefs :: (Text -> Text) -> FESequent -> FESequent
applyOnAllRefs func = replaceSteps (map f)
  where
    f (SubProof p) = SubProof (map f p)
    f (Line s r u a) = Line s r u (map func a)

-- | Parse rule argument and apply function if it's a line/subproof reference
applyOnLineNumberRef :: (Abs.Arg -> Integer -> Integer -> Text) -> Text -> Text
applyOnLineNumberRef f refText = case pArg (myLexer (unpack refText)) of
  Left {} -> refText
  Right arg@(Abs.ArgRange a b) -> f arg 0 a <> "-" <> f arg 1 b
  Right arg@(Abs.ArgLine l) -> f arg 0 l
  Right _ -> refText

addUDRToDocument :: FEUserDefinedRule -> FEDocument -> FEDocument
addUDRToDocument newUDR doc = doc & fedUserDefinedRules %~ f
  where
    f Nothing = Just [newUDR]
    f (Just udrs) = Just $ udrs ++ [newUDR]

removeUDRFromDocument :: Int -> FEDocument -> FEDocument
removeUDRFromDocument idx doc = doc & fedUserDefinedRules %~ f
  where
    f Nothing = Nothing
    f (Just udrs) = Just $ udrs ^.. folded . ifiltered (\i _ -> i /= idx)

editUDRInDocument :: Int -> (FEUserDefinedRule -> FEUserDefinedRule) -> FEDocument -> FEDocument
editUDRInDocument idx updateUDR doc = doc & fedUserDefinedRules %~ f
  where
    f Nothing = Nothing
    f (Just udrs) = Just $ udrs & element idx %~ updateUDR

editNameInUDR :: Text -> FEUserDefinedRule -> FEUserDefinedRule
editNameInUDR newName udr = udr & udrName .~ parsedName
  where parsedName = replaceSpecialSymbols newName

editPathInUDR :: FilePath -> FEUserDefinedRule -> FEUserDefinedRule
editPathInUDR newPath udr = udr & udrPath .~ newPath

editIOInUDR :: Maybe [FEFormula] -> Maybe FEFormula -> FEUserDefinedRule -> FEUserDefinedRule
editIOInUDR input output udr = udr
  & udrInput .~ input
  & udrOutput .~ output