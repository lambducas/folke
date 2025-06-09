{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use let" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module Frontend.Components.RenderProofTab (
  renderProofTab
) where

import Prelude hiding (span)

import Frontend.Types
import Frontend.Themes (getActualTheme)
import Frontend.Components.GeneralUIComponents
import Frontend.Helper.General (trimText, extractErrorMsg, getWarningsInSubProof, isErrorSubProof, isErrorLine, getWarningsOnLine, evalPath, maybeIndex, evalPathSafe, trimBeginning)
import Frontend.Parse (validateRuleArgument, parseRule, validateStatement, validateRule)
import Shared.Messages
import Shared.SpecialCharacters (replaceSpecialSymbolsInverse)
import Logic.Par (myLexer, pForm)

import Monomer
import Monomer.Widgets.Singles.Base.InputField (InputFieldState (_ifsCurrText, _ifsCursorPos))
import qualified Monomer.Lens as L
import Data.Text (Text, pack, intercalate, unpack, toLower, isInfixOf)
import qualified Data.Text (length)
import Data.Default (Default(def))
import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import Data.Set (fromList, toList)
import qualified Data.Map
import Control.Lens
import TextShow (showt)

import Monomer.Widgets.Containers.TextFieldSuggestions (textFieldSuggestionsV)
import Frontend.Helper.ProofHelper (getCurrentSequent)
-- import Frontend.Components.ProofRow

renderProofTab
  :: Bool
  -> WidgetEnv AppModel AppEvent
  -> AppModel
  -> File
  -> Text
  -> WidgetNode AppModel AppEvent
renderProofTab isMac _wenv model file _heading = cached where
  ruleAndArgWidth = 400

  ctrl = if isMac then "Cmd" else "Ctrl"

  selTheme = getActualTheme $ model ^. preferences . selectedTheme
  accentColor = selTheme ^. L.userColorMap . at "accent" . non def
  dividerColor = selTheme ^. L.userColorMap . at "dividerColor" . non def
  hoverColor = selTheme ^. L.userColorMap . at "hoverColor" . non def
  proofBoxColor = selTheme ^. L.userColorMap . at "proofBoxColor" . non def

  u = model ^. preferences . fontSize

  h1 = Frontend.Components.GeneralUIComponents.h1 model
  h2 = Frontend.Components.GeneralUIComponents.h2 model
  -- h3 = Frontend.Components.GeneralUIComponents.h3 model
  span = Frontend.Components.GeneralUIComponents.span model
  -- span_ = Frontend.Components.GeneralUIComponents.span_ model
  symbolSpan = Frontend.Components.GeneralUIComponents.symbolSpan model
  symbolSpan_ = Frontend.Components.GeneralUIComponents.symbolSpan_ model
  paragraph = Frontend.Components.GeneralUIComponents.paragraph model
  -- paragraph_ = Frontend.Components.GeneralUIComponents.paragraph_ model
  iconLabel = Frontend.Components.GeneralUIComponents.iconLabel model
  -- iconButton = Frontend.Components.GeneralUIComponents.iconButton model
  trashButton = Frontend.Components.GeneralUIComponents.trashButton model
  bold = Frontend.Components.GeneralUIComponents.bold model
  -- normalStyle = Frontend.Components.GeneralUIComponents.normalStyle model
  symbolStyle = Frontend.Components.GeneralUIComponents.symbolStyle model

  button = Frontend.Components.GeneralUIComponents.button model
  fastTooltip = Frontend.Components.GeneralUIComponents.fastTooltip model
  -- fastScroll = Frontend.Components.GeneralUIComponents.fastScroll
  fastVScroll = Frontend.Components.GeneralUIComponents.fastVScroll
  -- fastHScroll = Frontend.Components.GeneralUIComponents.fastHScroll

  -- textFieldStyle = [border 0 transparent]
  -- textFieldFocusStyle = [border 1 accentColor]
  -- textFieldV_ a b c = Monomer.textFieldV_ a b c
  --   `styleBasic` textFieldStyle
  --   `styleFocus` textFieldFocusStyle
  -- textFieldSuggestionsV a b c d e = Monomer.Widgets.Containers.TextFieldSuggestions.textFieldSuggestionsV a b c d e
  --   `styleBasic` textFieldStyle
  --   `styleFocus` textFieldFocusStyle

  cached = box_ [mergeRequired hasChanged, alignLeft] updated
  updated = renderProofTab'
  hasChanged _wenv old new =
      oldProofStatus /= newProofStatus ||
      -- oldHovered /= newHovered ||
      oldSeq /= newSeq ||
      (old ^. udrPopup && not (new ^. udrPopup)) ||
      old ^. persistentState . currentFile /= new ^. persistentState . currentFile ||
      old ^. preferences . warningMessageSeverity /= new ^. preferences . warningMessageSeverity
    where
      oldProofStatus = old ^. proofStatus
      -- oldHovered = old ^. hoveredProofLine
      oldSeq = getCurrentSequent old

      newProofStatus = new ^. proofStatus
      -- newHovered = new ^. hoveredProofLine
      newSeq = getCurrentSequent new

  renderProofTab' :: WidgetNode AppModel AppEvent
  renderProofTab' = maybe invalidProofTab validProofTab (_parsedDocument file)

  invalidProofTab = vstack [
      vstack [
        h1 $ pack $ _path file,
        spacer
      ] `styleBasic` [padding 10, borderB 1 dividerColor],
      fastVScroll (vstack [
        paragraph "Corrupt proof! Try editing the proof-file in a text editor to fix it. Close this tab and reopen the proof-file after corrupted data is removed",
        spacer,
        paragraph "File preview:",
        spacer,
        symbolSpan_ (getContent file) [multiline]
      ]) `styleBasic` [padding 10]
    ]
    where
      getContent (OtherFile {}) = _content file
      getContent (MarkdownFile {}) = _content file
      getContent (ProofFile {}) = _content file
      getContent _ = "Could not get content!";

  validProofTab parsedDocument = vstack [
      proofHeader parsedDocument,
      proofBody parsedDocument,
      proofFooter
    ]

  proofHeader parsedDocument = vstack [
      subheading
    ] `styleBasic` [padding 10, borderB 1 dividerColor]
    where
      subheading
        | null premises && conclusion == "" = span "Empty proof"
        | otherwise = symbolSpan_ prettySequent [ellipsis] `styleBasic` [height (1.25*u)]
      prettySequent = intercalate ", " premises <> " ⊢ " <> conclusion
      conclusion = _conclusion parsedSequent
      premises = _premises parsedSequent
      parsedSequent = _sequent parsedDocument

  proofBody document = box_ [alignLeft] $
    fastScroll $
      box_ [alignLeft] $
        proofBodyContent document
          `styleBasic` [paddingT 20, paddingL 20, minWidth 950]

  proofFooter = hstack [
      proofStatusLabel
    ] `styleBasic` [padding 10, borderT 1 dividerColor]

  proofStatusLabel = case model ^. proofStatus of
    Nothing -> span "Ready to check proof" `styleBasic` [textColor gray]
    Just (FEError warns _error) ->
      hstack_ [childSpacing_ 20] [
        bold $ span "Proof is incorrect" `styleBasic` [textColor red],
        nrWarningsNotice warns
      ]
    Just (FEOk warns) ->
      hstack_ [childSpacing_ 20] [
        bold $ span "Proof is correct" `styleBasic` [textColor lime],
        nrWarningsNotice warns
      ]
    where
      nrWarningsNotice warns = widgetIf (not (null warns)) $
        hstack_ [childSpacing_ 5] [
          iconLabel remixErrorWarningLine
            `styleBasic` [textMiddle, textColor orange],
          span (showt (length warns) <> " warning" <> plural)
            `styleBasic` [textColor orange]
        ]
        where
          plural = if length warns == 1
            then ""
            else "s"

  proofBodyContent :: FEDocument -> WidgetNode AppModel AppEvent
  proofBodyContent document = vstack [
      hgrid_ [childSpacing] [
        premiseWidget,
        conclusionWidget
      ]
        `styleBasic` [paddingR 20],
      spacer, spacer,

      proofWidget,

      -- Hack so last proof line can scroll all the way to the top
      box (label "") `styleBasic` [height 1000]
    ]
    where
      sequent = _sequent document

      premiseWidget = box_ [mergeRequired hasChanged, alignTop, alignLeft] $
        vstack_ [childSpacing] [
          h2 "Premises",
          vstack_ [childSpacing] $ zipWith premiseLine (_premises sequent) [0..],
          widgetIf (null $ _premises sequent) (span "No premises"),
          hstack [button "Add premise (Enter)" (AddPremise (nrPremises - 1)) `nodeKey` "addPremiseButton"]
        ]
        where
          hasChanged _wenv old new = oldPremises /= newPremises
            where
              oldSeq = getCurrentSequent old
              oldPremises = oldSeq >>= Just . _premises
              newSeq = getCurrentSequent new
              newPremises = newSeq >>= Just . _premises

      premiseLine premise idx = box_ [alignLeft] $ hstack [
          someKeystrokes [
            ("Up", FocusOnKey $ WidgetKey ("premise.input." <> showt (idx - 1)), True),
            ("Down", FocusOnKey $ WidgetKey ("premise.input." <> showt (idx + 1)), idx < nrPremises - 1),
            ("Down", FocusOnKey $ WidgetKey "conclusion.input", idx >= nrPremises - 1),
            ("Delete", RemovePremise idx, True),
            ("Enter", AddPremise idx, True)
          ] (symbolStyle $ textFieldV_ premise (EditPremise idx) [placeholder "Enter premise"]
            `nodeKey` ("premise.input." <> showt idx)
            `styleBasic` [styleIf isPremiseError (border 1 red)]),
          spacer,
          fastTooltip "Remove premise" $ trashButton (RemovePremise idx),
          spacer
        ]
          `nodeKey` ("premise.line." <> showt idx)
          -- `styleBasic` [maxWidth 400]
        where isPremiseError = trimText premise == "" || isLeft (pForm (myLexer (unpack (replaceSpecialSymbolsInverse premise))))

      conclusionWidget = box_ [mergeRequired hasChanged, alignTop, alignLeft] $
        vstack_ [childSpacing] [
          h2 "Conclusion",
          box_ [alignLeft] (
            someKeystrokes [
              ("Up", FocusOnKey $ WidgetKey ("premise.input." <> showt (nrPremises - 1)), nrPremises >= 1),
              ("Down", NextFocus 1, True),
              ("Enter", NextFocus 1, True)
            ] (symbolStyle $ textFieldV_ (_conclusion sequent) EditConclusion [placeholder "Enter conclusion here"]
              -- `styleBasic` [maxWidth 600]
              `styleBasic` [styleIf isError (border 1 red)]
              `nodeKey` "conclusion.input")
          )
        ]
        where
          isError = isLeft (pForm (myLexer (unpack (replaceSpecialSymbolsInverse (_conclusion sequent)))))

          hasChanged _wenv old new = oldNrPremises /= newNrPremises || oldConclusion /= newConclusion
            where
              oldSeq = getCurrentSequent old
              oldPremises = oldSeq >>= Just . _premises
              oldConclusion = oldSeq >>= Just . _conclusion
              oldNrPremises = fromMaybe (-1) (oldPremises >>= Just . length)

              newSeq = getCurrentSequent new
              newPremises = newSeq >>= Just . _premises
              newConclusion = newSeq >>= Just . _conclusion
              newNrPremises = fromMaybe (-1) (newPremises >>= Just . length)

      proofWidget = vstack_ [childSpacing] [
          h2 "Proof",
          hstack [
            lineNumbers,
            tree
          ],

          h2 "Summary"
            `styleBasic` [paddingT 20],
          errorWidget
        ]

      errorWidget = case model ^. proofStatus of
        Nothing -> span "Run Validate Proof to get a summary"
        Just (FEError warns error) ->
          vstack_ [childSpacing] [
            bold $ span "Proof is incorrect" `styleBasic` [textColor red],
            paragraph (pack $ show error) `styleBasic` [textColor red],
            renderWarningList warns
          ]
        Just (FEOk warns) ->
          vstack_ [childSpacing] [
            bold $ span "Proof is correct" `styleBasic` [textColor lime],
            renderWarningList warns
          ]
        where
          renderWarningList warns = vstack_ [childSpacing] (map (flip styleBasic [textColor orange] . paragraph . pack . show) warns)

      tree = vstack [
          ui,
          spacer,
          hstack_ [childSpacing] [
            fastTooltip "Insert new line below last line" $ button "+ New line" AddLine,
            fastTooltip "Insert subproof below last line" $ button "+☐ New subproof" AddSubProof
          ]
        ]
        where
          ui = vstack_ [childSpacing] (ghostPremises ++ map fst s)
          s = getSubProof (_steps sequent) [] 0 1

          ghostPremises = map ghostPremise (_premises sequent)
          ghostPremise premise = hstack [
              symbolSpan premise,
              filler,
              symbolSpan "premise" `styleBasic` [width ruleAndArgWidth, paddingH 10],
              spacer,
              vstack [] `styleBasic` [width 250]
            ] `styleBasic` [height 34]

      pfDropTarget idx = dropTarget_ ((\p -> MovePathToPath p idx) :: FormulaPath -> AppEvent) [dropTargetStyle hoverStyle]
        where hoverStyle = [borderB 3 accentColor, bgColor hoverColor]

      pf :: FEStep -> Integer -> FormulaPath -> (WidgetNode AppModel AppEvent, Integer)
      pf (SubProof p) index path = (ui, lastIndex)
        where
          ui = draggable_ path [transparency 0.3] $ vstack [
              hstack [
                pfDropTarget path (vstack [] `styleBasic` [width 24]),

                vstack_ [childSpacing] (map fst s)
              ]
                `styleBasic` [border 1 proofBoxColor, styleIf isWarning (border 1 orange), styleIf isError (border 1 red), borderR 0 transparent, paddingV 8],

              widgetIf isError ((span . pack . extractErrorMsg) (model ^. proofStatus))
                `styleBasic` [textColor red, paddingT (0.5*u), logicTextFont model],

              vstack (map warningLabel warnings)
            ]
          isWarning = not (null warnings)
          warnings = getWarningsInSubProof rStart rEnd (model ^. proofStatus)
          isError = isErrorSubProof rStart rEnd (model ^. proofStatus)
          rStart = index + toInteger (length (_premises sequent))
          rEnd = lastIndex - 1 + toInteger (length (_premises sequent))
          lastIndex = if null s then index else snd $ last s
          s = getSubProof p path 0 index

      pf (Line statement rule usedArguments arguments) index path = (ui, lastIndex)
        where
          hasChanged _wenv old new =
              -- oldHovered == l || newHovered == l ||
              oldProofStatus /= newProofStatus ||
              old ^. persistentState . currentFile /= new ^. persistentState . currentFile ||
              oldNrPremises /= newNrPremises ||
              oldStep /= newStep
            where
              -- l = fromIntegral lineNumber

              oldProofStatus = old ^. proofStatus
              -- oldHovered = old ^. hoveredProofLine
              oldSeq = getCurrentSequent old
              oldPremises = oldSeq >>= Just . _premises
              oldNrPremises = fromMaybe (-1) (oldPremises >>= Just . length)
              oldStep = oldSeq >>= evalPathSafe path

              newProofStatus = new ^. proofStatus
              -- newHovered = new ^. hoveredProofLine
              newSeq = getCurrentSequent new
              newPremises = newSeq >>= Just . _premises
              newNrPremises = fromMaybe (-1) (newPremises >>= Just . length)
              newStep = newSeq >>= evalPathSafe path

          ui = (box_ [mergeRequired hasChanged] $
            pfDropTarget path $
              draggable_ path [transparency 0.3] $
                box_ [onBtnReleased handleBtn, expandContent] $
                  vstack [
              -- box_ [
              --       onEnter (SetHoveredProofLine (fromIntegral lineNumber))
              --       -- onLeave (SetHoveredProofLine (-1))
              --     ] $
                  hstack [
                hstack [
                  firstKeystroke [
                    ("Up", FocusOnKey $ WidgetKey (showt (index - 1) <> ".statement"), prevIndexExists),
                    ("Up", FocusOnKey $ WidgetKey "conclusion.input", not prevIndexExists),
                    ("Down", FocusOnKey $ WidgetKey (showt (index + 1) <> ".statement"), nextIndexExists),
                    -- ("Right", FocusOnKey $ WidgetKey (showt index <> ".rule"), True),

                    ("Ctrl-Tab", SwitchLineToSubProof path (WidgetKey $ showt index <> ".statement"), True),
                    ("Ctrl-Shift-Tab", SwitchSubProofToLine pathToParentSubProof (WidgetKey $ showt index <> ".statement"), True),
                    ("Delete", RemoveLine False path, trashActive),
                    ("Backspace", RemoveLine False path, canBackspaceToDelete),
                    (ctrl <> "-Enter", InsertLineAfter False path, not isLastLine || not nextIndexExists),
                    (ctrl <> "-Enter", InsertLineAfter False pathToParentSubProof, isLastLine),
                    ("Enter", NextFocus 1, True)
                  ] (symbolStyle $ textFieldV_ statement (EditFormula path)
                      [onKeyDown handleFormulaKey, placeholder "Empty statement", textFieldPutCursorAtFirstMissmatch]
                    `styleBasic` [styleIf isWarning (border 1 orange)]
                    `styleBasic` [styleIf isStatementError (border 1 red)]
                    `nodeKey` (showt index <> ".statement"))
                    `nodeKey` (showt index <> ".statement.keystroke"),

                  spacer,

                  hstack_ [childSpacing] [
                    -- ruleKeystrokes ruleField

                    -- ruleKeystrokes $ symbolStyle $ textFieldSuggestionsV rule (\_i t -> EditRuleName path t) (allRules rule) (const ruleField) label
                    --   `styleBasic` [styleIf isWarning (border 1 orange)]
                    --   `styleBasic` [styleIf isRuleError (border 1 red)],

                    symbolStyle $ textFieldSuggestionsV rule (\_i t -> EditRuleName path t) (allRules rule) (const (ruleKeystrokes ruleField)) label
                      `styleBasic` [styleIf isWarning (border 1 orange)]
                      `styleBasic` [styleIf isRuleError (border 1 red)],

                    argInputs
                  ]
                    `styleBasic` [width ruleAndArgWidth]
                ],
                spacer,
                b -- `nodeVisible` (model ^. hoveredProofLine == fromIntegral lineNumber),
                -- Keep spacing when buttons are hidden
                -- label ""
                --   `styleBasic` [width 250]
                --   `nodeVisible` (model ^. hoveredProofLine /= fromIntegral lineNumber)
              ],

              widgetIf isError ((span . pack . extractErrorMsg) (model ^. proofStatus))
                `styleBasic` [textColor red, paddingT (0.5*u), logicTextFont model],

              vstack (map warningLabel warnings)
            ]
              `nodeKey` showt index) `nodeKey` (showt index <> pack (show path) <> ".mergeBox")

          ruleKeystrokes w = firstKeystroke [
              -- ("Up", FocusOnKey $ WidgetKey (showt (index - 1) <> ".rule"), prevIndexExists),
              -- ("Up", FocusOnKey $ WidgetKey "conclusion.input", not prevIndexExists),
              -- ("Down", FocusOnKey $ WidgetKey (showt (index + 1) <> ".rule"), nextIndexExists),
              -- ("Left", FocusOnKey $ WidgetKey (showt index <> ".statement"), True),

              ("Ctrl-Tab", SwitchLineToSubProof path (WidgetKey $ showt index <> ".rule"), True),
              ("Ctrl-Shift-Tab", SwitchSubProofToLine pathToParentSubProof (WidgetKey $ showt index <> ".rule"), True),
              ("Delete", RemoveLine False path, trashActive),
              ("Backspace", FocusOnKey (WidgetKey (showt index <> ".statement")), rule == ""),
              (ctrl <> "-Enter", InsertLineAfter False path, not isLastLine),
              (ctrl <> "-Enter", InsertLineAfter False pathToParentSubProof, isLastLine),
              ("Enter", NextFocus 1, usedArguments > 0),
              ("Enter", InsertLineAfter False path, usedArguments == 0)
            ] w
              `nodeKey` (showt index <> ".rule.keystroke")

          ruleField = symbolStyle $ textFieldV_ rule (EditRuleName path)
            [onKeyDown handleRuleNameKey, placeholder "No rule", selectOnFocus]
              `styleBasic` [styleIf isWarning (border 1 orange)]
              `styleBasic` [styleIf isRuleError (border 1 red)]
              `nodeKey` (showt index <> ".rule")

          argInputs = widgetIf (usedArguments /= 0) $ hstack_ [childSpacing] (zipWith argInput (take usedArguments arguments) [0..])
          argInput argument idx = hstack [
              symbolSpan currentLabel,
              firstKeystroke [
                ("Up", FocusOnKey $ WidgetKey (showt (index - 1) <> ".ruleArg." <> showt idx), prevIndexExists),
                ("Up", FocusOnKey $ WidgetKey "conclusion.input", not prevIndexExists),
                ("Down", FocusOnKey $ WidgetKey (showt (index + 1) <> ".ruleArg." <> showt idx), nextIndexExists),
                ("Ctrl-Tab", SwitchLineToSubProof path (WidgetKey $ showt index <> ".ruleArg." <> showt idx), True),
                ("Ctrl-Shift-Tab", SwitchSubProofToLine pathToParentSubProof (WidgetKey $ showt index <> ".ruleArg." <> showt idx), True),
                ("Delete", RemoveLine False path, trashActive),
                ("Backspace", FocusOnKey (WidgetKey (showt index <> ".rule")), isFirstArg && argument == ""),
                ("Backspace", FocusOnKey (WidgetKey (showt index <> ".ruleArg." <> showt (idx - 1))), not isFirstArg && argument == ""),
                (ctrl <> "-Enter", InsertLineAfter False path, not isLastLine || not nextIndexExists),
                (ctrl <> "-Enter", InsertLineAfter False pathToParentSubProof, isLastArg && isLastLine),
                ("Enter", InsertLineAfter False path, isLastArg),
                ("Enter", NextFocus 1, not isLastArg)
              ] (symbolStyle $ textFieldV_ trimmedArgument (EditRuleArgument path idx)
                [onKeyDown (handleRuleArgKey idx), placeholder ("Arg. " <> showt (index + 1)), selectOnFocus]
                  `nodeKey` (showt index <> ".ruleArg." <> showt idx)
                  `styleBasic` [width currentWidth]
                  `styleBasic` [styleIf isWarning (border 1 orange)]
                  `styleBasic` [styleIf isRuleArgError (border 1 red)])
            ]
            where
              trimmedArgument
                | parseRule rule == "EqE" = trimBeginning "u:=" argument
                | otherwise = argument

              isFirstArg = idx == 0
              isLastArg = idx + 1 == usedArguments
              isRuleArgError = isError || not (validateRuleArgument argument)

              currentLabel = fromMaybe "" $ maybeIndex labels idx
              labels = case md of
                Nothing -> repeat ""
                Just (RuleMetaData {_argumentLabels=l}) -> l

              currentWidth = fromMaybe 70 $ maybeIndex widths idx
              widths = case md of
                Nothing -> repeat 70
                Just (RuleMetaData _ _ Nothing) -> repeat 70
                Just (RuleMetaData _ _ (Just ws)) -> map (fromMaybe 70) ws

              md = Data.Map.lookup (parseRule rule) ruleMetaDataMap

          b = box $ hstack_ [childSpacing] [
                fastTooltip "Remove line (Delete)" $
                  trashButton (RemoveLine False path)
                    `nodeEnabled` trashActive,

                vstack [
                  fastTooltip "Insert line above" $
                    button "↑+" (InsertLineBefore False path) `styleBasic` [textSize (0.75*u), width 50, height 17, padding 0, radiusBL 0, radiusBR 0],
                  fastTooltip "Insert line below (Enter)" $
                    button "↓+" (InsertLineAfter False path) `styleBasic` [textSize (0.75*u), width 50, height 17, padding 0, radiusTL 0, radiusTR 0, borderT 1 dividerColor]
                ] `styleBasic` [height 34],

                -- fastTooltip "Insert subproof below" $
                --   button "↓☐+" (InsertSubProofAfter path),

                fastTooltip ("Convert line to subproof (" <> ctrl <> "+Tab)") $
                  button "→☐" (SwitchLineToSubProof path (WidgetKey $ showt index <> ".statement")),

                widgetIf isSubProofSingleton $
                  fastTooltip ("Undo subproof (" <> ctrl <> "+Shift+Tab)") $
                    button "☒" (SwitchSubProofToLine pathToParentSubProof (WidgetKey $ showt index <> ".statement")),

                widgetIf (isLastLine && nextIndexExists) $
                  fastTooltip ("Close subproof (" <> ctrl <> "+Enter)") $
                    button "⏎" (InsertLineAfter False pathToParentSubProof)

                -- widgetIf isLastLine (button "/[]+" (InsertSubProofAfter pathToParentSubProof))
              ] `styleBasic` [width 250]

          -- allRules = map fst visualRuleNames
          allRules rule = (toList . fromList) $ filter (\f -> toLower rule `isInfixOf` toLower f) (map snd visualRuleNames) ++ ["" | rule == ""]
          -- allRules = (toList . fromList) $ replaceSpecialSymbols rule : filter (\f -> (replaceSpecialSymbols . toLower) rule `isInfixOf` toLower f) ((map fst visualRuleNames) ++ (map snd visualRuleNames))
          -- allRules = [replaceSpecialSymbols rule] ++ (filter (\f -> (replaceSpecialSymbols . toLower) rule `isInfixOf` toLower f) $ map (pack . fst) (Data.Map.toList $ rules newEnv))--[model ^. userLens, "Thecoder", "another", "bruh", "tesdt", "dsjhnsifhbsgfsghffgusgfufgssf", "1", "2"]
          --   where rules (Env _ _ r _ _ _ _ _) = r

          handleBtn BtnRight _ = OpenContextMenu ctxProofLine
          handleBtn _ _ = NoEvent

          ctxProofLine = [
              ("Remove Line", "", RemoveLine False path, trashActive),
              ("Remove Line (Update refs)", "", RemoveLine True path, trashActive),

              ("Insert Line Above", "", InsertLineBefore False path, True),
              ("Insert Line Above (Update refs)", "", InsertLineBefore True path, True),

              ("Insert Line Below", "", InsertLineAfter False path, True),
              ("Insert Line Below (Update refs)", "", InsertLineAfter True path, True),

              ("Insert Subproof Above", "", InsertSubProofBefore False path, True),
              ("Insert Subproof Above (Update refs)", "", InsertSubProofBefore True path, True),

              ("Insert Subproof Below", "", InsertSubProofAfter False path, True),
              ("Insert Subproof Below (Update refs)", "", InsertSubProofAfter True path, True),

              ("Insert Line After Subproof", "", InsertLineAfter False pathToParentSubProof, isLastLine && nextIndexExists),
              ("Insert Line After Subproof (Update refs)", "", InsertLineAfter True pathToParentSubProof, isLastLine && nextIndexExists),

              ("Insert Subproof After Subproof", "", InsertSubProofAfter False pathToParentSubProof, isLastLine && nextIndexExists),
              ("Insert Subproof After Subproof (Update refs)", "", InsertSubProofAfter True pathToParentSubProof, isLastLine && nextIndexExists),

              ("Convert Line to Subproof", "", SwitchLineToSubProof path (WidgetKey $ showt index <> ".statement"), True),
              ("Undo Subproof", "", SwitchSubProofToLine pathToParentSubProof (WidgetKey $ showt index <> ".statement"), isSubProofSingleton)
            ]

          handleFormulaKey, handleRuleNameKey :: (KeyMod, KeyCode, InputFieldState Text) -> AppEvent
          handleFormulaKey (_mod, code, state)
            | isKeyRight code && isAtEnd = FocusOnKey $ WidgetKey (showt index <> ".rule")
            | otherwise = NoEvent
            where
              isAtEnd = cursorPos == textLen
              cursorPos = _ifsCursorPos state
              textLen = Data.Text.length (_ifsCurrText state)

          handleRuleNameKey (_mod, code, state)
            | isKeyLeft code && isAtBeginning = FocusOnKey $ WidgetKey (showt index <> ".statement")
            | isKeyRight code && isAtEnd = FocusOnKey $ WidgetKey (showt index <> ".ruleArg.0")
            | otherwise = NoEvent
            where
              isAtEnd = cursorPos == textLen
              textLen = Data.Text.length (_ifsCurrText state)
              isAtBeginning = cursorPos == 0
              cursorPos = _ifsCursorPos state

          handleRuleArgKey :: Int -> (KeyMod, KeyCode, InputFieldState Text) -> AppEvent
          handleRuleArgKey argIdx (_mod, code, state)
            | isKeyLeft code && isAtBeginning && argIdx == 0 = FocusOnKey $ WidgetKey (showt index <> ".rule")
            | isKeyLeft code && isAtBeginning && argIdx /= 0 = FocusOnKey $ WidgetKey (showt index <> ".ruleArg." <> showt (argIdx - 1))
            | isKeyRight code && isAtEnd && argIdx + 1 < usedArguments = FocusOnKey $ WidgetKey (showt index <> ".ruleArg." <> showt (argIdx + 1))
            | otherwise = NoEvent
            where
              isAtEnd = cursorPos == textLen
              textLen = Data.Text.length (_ifsCurrText state)
              isAtBeginning = cursorPos == 0
              cursorPos = _ifsCursorPos state

          isStatementError = isError || not (validateStatement statement)
          isRuleError = isError || not (validateRule document rule)
          isError = isErrorLine lineNumber (model ^. proofStatus)
          isWarning = not (null warnings)
          warnings = getWarningsOnLine lineNumber (model ^. proofStatus)
          lineNumber = index + toInteger (length (_premises sequent))
          canBackspaceToDelete = rule == "" && statement == "" && trashActive
          trashActive = not (index == 1 && not nextIndexExists && statement == "" && rule == "")
          pathToParentSubProof = init path
          parentSubProof = evalPath pathToParentSubProof sequent
          lastIndex = index + 1
          prevIndexExists = index > 1
          nextIndexExists = not (isLastLine && length path == 1)
          isSubProofSingleton = length path /= 1 && isSingleton parentSubProof
          isSingleton (SubProof p) = length p == 1
          isSingleton _ = False
          isLastLine = case parentSubProof of
            SubProof p -> length p == last path + 1
            _ -> False

      warningLabel w = span (pack w) `styleBasic` [textColor orange, paddingT (0.5*u), logicTextFont model]

      getSubProof p path arrayIndex visualIndex
        | arrayIndex < length p = u : getSubProof p path (arrayIndex + 1) (snd u)
        | otherwise = []
          where u = pf (p !! arrayIndex) visualIndex (path ++ [arrayIndex])

      lineNumbers = ui
        where
          ui = vstack_ [childSpacing] (ghostLines ++ map fst s)
          s = getSubProof2 (_steps sequent) [] 0 (length (_premises sequent) + 1)

          ghostLines = map ghostLine [1..length (_premises sequent)]
          ghostLine index = symbolSpan (showt index <> ".")
            `styleBasic` [width 48, paddingR 12, height 34, textRight]

      ln (SubProof p) index path = (ui, lastIndex)
        where
          ui = vstack [
              vstack_ [childSpacing] (map fst s) `styleBasic` [border 1 transparent, paddingV 8],

              widgetIf isError (span " ")
                `styleBasic` [textColor red, paddingT (0.5*u)],

              vstack (map (const $ warningLabel (" " :: String)) warnings)
            ]
          warnings = getWarningsInSubProof rStart rEnd (model ^. proofStatus)
          isError = isErrorSubProof rStart rEnd (model ^. proofStatus)
          rStart = toInteger index
          rEnd = toInteger $ lastIndex - 1
          lastIndex = if null s then index else snd $ last s
          s = getSubProof2 p path 0 index

      ln (Line {}) index _path = (ui, lastIndex)
        where
          ui = vstack [
              symbolSpan (showt index <> ".")
                `styleBasic` [width 48, paddingR 12, height 34, textRight, styleIf isError (textColor red)]
                `nodeKey` showt index <> "label",

              widgetIf isError (span " ")
                `styleBasic` [textColor red, paddingT (0.5*u)],

              vstack (map (const $ warningLabel (" " :: String)) warnings)
            ]
          lastIndex = index + 1
          lineNumber = toInteger index
          isError = isErrorLine lineNumber (model ^. proofStatus)
          warnings = getWarningsOnLine lineNumber (model ^. proofStatus)
          warningLabel _ = span "" `styleBasic` [textColor orange, paddingT (0.5*u)]

      getSubProof2 p path arrayIndex visualIndex
        | arrayIndex < length p = u : getSubProof2 p path (arrayIndex + 1) (snd u)
        | otherwise = []
          where u = ln (p !! arrayIndex) visualIndex (path ++ [arrayIndex])

      nrPremises = length (_premises sequent)