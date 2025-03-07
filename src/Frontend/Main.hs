{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.Main where

import System.Directory
import Control.Concurrent (threadDelay, Chan, newChan, writeChan, readChan)
import Control.Lens
import Monomer
import TextShow
import Data.Text (Text, replace, unpack, pack, intercalate)
import qualified Data.Maybe

import Shared.Messages
import Backend.TypeChecker (handleFrontendMessage, isProofCorrect)
import Parser.Logic.Abs (Sequent(..), Step(..), Form(..), Pred(..), PredId(..), Params(..), RuleId(..))
import Frontend.Communication (startCommunication, evaluateProofSegment, evaluateProofStep)

type FormulaPath = [Int]

data ProofFormula
  = MainProof [ProofFormula]
  | Formula {
    _statement :: Text,
    _rule :: Text
  }
  | SubProof [ProofFormula]
  | Removed
  deriving (Eq, Show)

data File = File {
  _path :: FilePath,
  _name :: Text,
  _subname :: Text,
  _content :: Text
} deriving (Eq, Show)

data AppModel = AppModel {
  _clickCount :: Int,

  _newFilePopupOpen :: Bool,
  _newFileName :: Text,
  _loadedFiles :: [File],
  _openFiles :: [File],
  _currentFile :: Maybe File,

  _conclusion :: Text,
  _proofFormulas :: ProofFormula,

  _frontendChan :: Chan FrontendMessage,
  _backendChan :: Chan BackendMessage,
  _proofStatus :: Maybe Bool
} deriving (Eq)

instance Show AppModel where
  show model = "AppModel { _clickCount = " ++ show (_clickCount model) ++
               ", _newFilePopupOpen = " ++ show (_newFilePopupOpen model) ++
               ", _newFileName = " ++ show (_newFileName model) ++
               ", _loadedFiles = " ++ show (_loadedFiles model) ++
               ", _openFiles = " ++ show (_openFiles model) ++
               ", _currentFile = " ++ show (_currentFile model) ++
               ", _conclusion = " ++ show (_conclusion model) ++
               ", _proofFormulas = " ++ show (_proofFormulas model) ++
               ", _proofStatus = " ++ show (_proofStatus model) ++ " }"

data AppEvent
  = AppInit
  | AppIncrease
  | NextFocus Int
  | AddLine
  | RemoveLine FormulaPath
  | EditLine FormulaPath Int Text
  | OutdentLine Int
  | IndentLine Int
  | SetLoadedFiles [File]
  | OpenFile File
  | CloseFile File
  | SetCurrentFile File
  | OpenCreateProofPopup
  | CreateEmptyProof Text

  | CheckProof
  | BackendResponse BackendMessage
  deriving (Eq, Show)

makeLenses 'File
makeLenses 'AppModel

h1 :: Text -> WidgetNode s e
h1 t = label t `styleBasic` [ textSize 24, textFont "Bold" ]

iconButton iconIdent action = button iconIdent action
  `styleBasic` [textFont "Remix", textMiddle, textColor orangeRed, bgColor transparent, border 0 transparent]

trashButton action = iconButton remixDeleteBinFill action

type SymbolDict = [(Text, Text)]

symbolLookup :: SymbolDict
symbolLookup = [
  ("!", "¬"),
  ("->", "→"),
  ("&", "∧"),
  ("|", "∨"),
  ("bot", "⊥"),
  ("#", "⊥"),
  ("forall", "∀"),
  ("exists", "∃"),
  ("=/>", "⊬"),
  ("=>", "⊢")
  ]

replaceFromLookup :: Text -> SymbolDict -> Text
replaceFromLookup s [] = s
replaceFromLookup s ((key, value):ls) = replace key value $ replaceFromLookup s ls

replaceSpecialSymbols :: Text -> Text
replaceSpecialSymbols s = replaceFromLookup s symbolLookup

tabs :: Int -> Text
tabs n = pack $ replicate n '\t'

-- Empty for now
exportProof :: AppModel -> Sequent
exportProof = const $ Seq [] FormBot []

-- exportProof model = Seq [] (FormPred (Pred (PredId (unpack (model ^. conclusion))) (Params []))) (map toStep (model ^. proofLines))

-- toStep :: ProofLine -> Step
-- toStep line = StepPrem (FormPred (Pred (PredId (unpack (line ^. statement))) (Params [])))


-- exportProof :: AppModel -> Text
-- exportProof model = "conclusion: " <> model ^. conclusion <> "\n" <> exportProofHelper (model ^. proofFormulas) 0

-- exportProofHelper :: ProofFormula -> Int -> Text
-- exportProofHelper (MainProof p) indent = exportProofHelper (SubProof p) indent
-- exportProofHelper (SubProof p) indent = tabs indent <> "{\n" <> intercalate "\n" (map (`exportProofHelper` (indent + 1)) p) <> "\n" <> tabs indent <> "}"
-- exportProofHelper (Formula statement rule) indent = tabs indent <> statement <> " : " <> rule <> ";"

-- isProofCorrect :: Text -> Bool
-- isProofCorrect p = True

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = hstack [
      fileWindow,
      editWindow
    ]

  fileWindow = vstack [
      box (label "Manage proofs") `styleBasic` [padding 10],
      vstack $ map fileItem (model ^. loadedFiles),
      spacer,

      box $ button "+ New proof" OpenCreateProofPopup `styleBasic` [padding 10],
      popup newFilePopupOpen (vstack [
        label "This will appear on top of the widget tree",
        spacer,
        textField newFileName,
        spacer,
        let cep = (CreateEmptyProof $ model ^. newFileName) in
          keystroke [("Enter", cep)] $ button "Create proof" cep
      ] `styleBasic` [bgColor gray, padding 10])
    ] `styleBasic` [ width 250, borderR 1 gray ]

  fileItem file = box_ [expandContent, onClick (OpenFile file)] $ vstack [
      label $ _name file,
      label $ _subname file
    ] `styleBasic` [borderB 1 gray, padding 8, bgColor darkGray, cursorHand]

  fileNavBar files = hscroll (hstack (map boxedLabel files))
    `styleBasic` [bgColor black, maxHeight 50, minHeight 50]
    where
      boxedLabel f = hstack [
          button (_name f) (SetCurrentFile f) `styleBasic` [textColor white, bgColor transparent, paddingV 8, paddingH 16, radius 0, border 0 transparent],
          button "x" (CloseFile f) `styleBasic` [textColor white, bgColor transparent, radius 0, border 0 transparent]
        ] `styleBasic` [bgColor darkGray, border 1 gray, styleIf isCurrent (bgColor darkSlateGray), styleIf isCurrent (borderB 0 transparent)]
          where isCurrent = (model ^. currentFile) == Just f

  editWindow = vstack [
      fileNavBar (model ^. openFiles),
      widgetIf (Data.Maybe.isJust (model ^. currentFile)) (proofWindow $ model ^. currentFile)
    ]

  proofWindow Nothing = label "No proof selected"
  proofWindow (Just file) = vstack [
      h1 $ _name file,
      label $ _subname file,
      spacer,
      -- label "→ ¬ ∧ ∨ ⊕ ⊥ ∀ ∃ ⊢ ⊬ ⟛",
      -- label $ replaceSpecialSymbols "P -> Q && L",
      -- vscroll $ label_ (exportProof model) [multiline],

      hstack [
        label "Conclusion",
        spacer,
        textField conclusion
      ] `styleBasic` [paddingV 8],
      spacer,

      vscroll $ vstack [
        fst $ proofTreeUI (model ^. proofFormulas),
        spacer,
        button "+ New line" AddLine `styleBasic` [ maxWidth 150 ]
      ],
      spacer,

      hstack [
        widgetIf (model ^. proofStatus == Just True) (label "Proof is correct :)" `styleBasic` [textColor lime]),
        widgetIf (model ^. proofStatus == Just False) (label "Proof is not correct!" `styleBasic` [textColor pink]),
        widgetIf (model ^. proofStatus == Nothing) (label "Checking proof..." `styleBasic` [textColor orange])
      ]
    ] `styleBasic` [padding 10]

  proofTreeUI :: ProofFormula -> (WidgetNode AppModel AppEvent, Integer)
  proofTreeUI formulas = pf formulas 1 []
    where
      pf :: ProofFormula -> Integer -> FormulaPath -> (WidgetNode AppModel AppEvent, Integer)
      pf (MainProof formulas) index path = (ui, lastIndex)
        where
          ui = vstack_ [childSpacing] (map fst s)
          lastIndex = if null s then index else snd $ last s
          s = getSubProof formulas path 0 index

      pf (SubProof p) index path = (ui, lastIndex)
        where
          ui = vstack_ [childSpacing] (map fst s) `styleBasic` [border 1 white, paddingV 8, paddingL 24]
          lastIndex = if null s then index else snd $ last s
          s = getSubProof p path 0 index

      pf (Formula statement rule) index path = (ui, lastIndex)
        where
          ui = hstack [
              label $ showt index <> ".",
              spacer,

              textFieldV (replaceSpecialSymbols statement) (EditLine path 0) `styleBasic` [textFont "Symbol_Regular"],
              spacer,

              textFieldV (replaceSpecialSymbols rule) (EditLine path 1) `styleBasic` [textFont "Symbol_Regular", width 175],
              spacer,

              trashButton (RemoveLine path)
              -- label $ showt index <> ".  " <> statement <> " : " <> rule
            ]
          lastIndex = index + 1

      getSubProof p path arrayIndex visualIndex
        | arrayIndex < length p = u : getSubProof p path (arrayIndex + 1) (snd u)
        | otherwise = []
          where u = pf (p !! arrayIndex) visualIndex (path ++ [arrayIndex])

directoryFilesProducer :: (AppEvent -> IO ()) -> IO ()
directoryFilesProducer sendMsg = do
  all <- listDirectory "./myProofs"
  sendMsg (SetLoadedFiles $ map packFile all)
  threadDelay $ 2 * seconds
  directoryFilesProducer sendMsg
    where
      seconds = 1000 * 1000
      packFile path = File path name subname content
        where
          name = pack path
          subname = "subname"
          content = "This is the content"

evaluateCurrentProof :: AppModel -> IO (Either String Sequent)
evaluateCurrentProof model = do
    let sequent = exportProof model
    evaluateProofSegment (model ^. frontendChan) (model ^. backendChan) sequent

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [
      Producer directoryFilesProducer,
      Task $ do
        frontendChan <- newChan
        backendChan <- newChan
        startCommunication frontendChan backendChan
        return $ BackendResponse (OtherBackendMessage "Initialized")
    ]
  AppIncrease -> [Model (model & clickCount +~ 1)]

  -- Bruh
  NextFocus 1 -> [
      MoveFocusFromKey Nothing FocusFwd
    ]
  NextFocus 4 -> [
      MoveFocusFromKey Nothing FocusFwd,
      MoveFocusFromKey Nothing FocusFwd,
      MoveFocusFromKey Nothing FocusFwd,
      MoveFocusFromKey Nothing FocusFwd
    ]

  AddLine -> [
      Model $ model & proofFormulas .~ addLine (model ^. proofFormulas)
    ]
    where
      addLine (MainProof p) = MainProof $ p ++ [newLine]
      newLine = Formula "" ""

  RemoveLine path -> [
      Model $ model & proofFormulas .~ removeLine path (model ^. proofFormulas)
    ]
    where
      removeLine removePath p = rl removePath [] p
      rl removePath currentPath (MainProof p)
        | removePath == currentPath = Removed
        | otherwise = MainProof $ filterRemoved $ zipWith (\p idx -> rl removePath (currentPath ++ [idx]) p) p [0..]
      rl removePath currentPath (SubProof p)
        | removePath == currentPath = Removed
        | otherwise = SubProof $ filterRemoved $ zipWith (\p idx -> rl removePath (currentPath ++ [idx]) p) p [0..]
      rl removePath currentPath f@(Formula _ _)
        | removePath == currentPath = Removed
        | otherwise = f
      filterRemoved p = filter (/=Removed) p

  EditLine path arg newText -> [
      Model $ model & proofFormulas .~ editLine path arg newText (model ^. proofFormulas)
    ]
    where
      editLine editPath arg newText formulas = el editPath arg newText [] formulas
      el editPath arg newText currentPath (MainProof p) = MainProof $ zipWith (\p idx -> el editPath arg newText (currentPath ++ [idx]) p) p [0..]
      el editPath arg newText currentPath (SubProof p) = SubProof $ zipWith (\p idx -> el editPath arg newText (currentPath ++ [idx]) p) p [0..]
      el editPath arg newText currentPath f@(Formula statement rule)
        | editPath == currentPath = case arg of
          0 -> Formula newText rule
          1 -> Formula statement newText
        | otherwise = f

  OpenCreateProofPopup -> [
      Model $ model & newFilePopupOpen .~ True
    ]

  CreateEmptyProof fileName -> [
      Producer (\_ -> writeFile ("./myProofs/" <> unpack fileName <> ".logic") ""),
      Model $ model
        & newFilePopupOpen .~ False
        & newFileName .~ ""
    ]

  SetLoadedFiles fs -> [
      Model $ model & loadedFiles .~ fs
    ]

  OpenFile f -> [
      Model $ model
        & openFiles .~ (model ^. openFiles ++ [f | f `notElem` model ^. openFiles])
        & currentFile ?~ f
        -- & proofFormulas .~ MainProof []
    ]

  CloseFile f -> [
      Model $ model
        & openFiles .~ filter (f/=) (model ^. openFiles)
        & currentFile .~ (if c == Just f then maybeHead (model ^. openFiles) else c)
    ]
    where c = model ^. currentFile

  SetCurrentFile f -> [ Model $ model & (currentFile ?~ f) ]

  BackendResponse (SequentChecked result) -> case result of
    Left err -> [Message "Error" (pack err)]  -- Add type annotation
    Right sequent -> [Model $ model & proofStatus .~ Just (isProofCorrect sequent)]

  BackendResponse (StepChecked result) -> case result of
    Left err -> [Message "Error" (pack err)]  -- Add type annotation
    Right step -> [Message "Step Status" ("Step is correct" :: Text)]  -- Add type annotation

  f@_ -> [
      Producer $ (\_ -> print f)
    ]

main :: IO ()
main = do
  frontendChan <- newChan
  backendChan <- newChan
  startCommunication frontendChan backendChan
  startApp (model frontendChan backendChan) handleEvent buildUI config
  where
    config = [
      appWindowTitle "● proof.logic - Proof Editor",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,

      appFontDef "Regular" "./assets/fonts/MPLUS1p/MPLUS1p-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/MPLUS1p/MPLUS1p-Medium.ttf",
      appFontDef "Bold" "./assets/fonts/MPLUS1p/MPLUS1p-Bold.ttf",

      appFontDef "Symbol_Regular" "./assets/fonts/JuliaMono/JuliaMono-Regular.ttf",
      appFontDef "Symbol_ Medium" "./assets/fonts/JuliaMono/JuliaMono-Medium.ttf",
      appFontDef "Symbol_Bold" "./assets/fonts/JuliaMono/JuliaMono-Bold.ttf",

      appFontDef "Remix" "./assets/fonts/remixicon.ttf",

      appInitEvent AppInit,
      appModelFingerprint show
      ]
    model frontendChan backendChan = AppModel {
      _clickCount = 0,
      _newFileName = "",
      _newFilePopupOpen = False,
      _loadedFiles = [],
      _openFiles = [],
      _currentFile = Nothing,
      _conclusion = "((P -> Q) && (!R -> !Q)) -> (P -> R)",
      _proofFormulas = MainProof [
        SubProof [
          Formula "(P -> Q) && (!R -> !Q)" "Assumption",
          SubProof [
            Formula "P" "Assumption",
            Formula "(P -> Q) && (!R -> !Q)" "1, Reiteration",
            Formula "P -> Q" "3, ||E",
            Formula "Q" "2, 4, ->E",
            Formula "!R -> !Q" "3, &&E",
            SubProof [
              Formula "!R" "Assumption",
              Formula "!R -> !Q" "6, Reiteration",
              Formula "!Q" "7, 8, ->E",
              Formula "Q" "5, Reiteration"
            ],
            Formula "!!R" "7-10, !I",
            Formula "R" "11, !!E"
          ],
          Formula "P -> R" "2-12, ->I"
        ],
        Formula "((P -> Q) && (!R -> !Q)) -> (P -> R)" "1-13, ->I"
      ],

      _frontendChan = frontendChan,
      _backendChan = backendChan,
      _proofStatus = Nothing  -- Initialize proof status
    }

removeIdx :: Int -> [a] -> [a]
removeIdx idx lst = part1 ++ drop 1 part2 where
  (part1, part2) = splitAt idx lst

-- https://www.youtube.com/watch?v=aS8O-F0ICxw
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (h:t) = Just h