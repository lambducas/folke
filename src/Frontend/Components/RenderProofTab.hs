{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use let" #-}

module Frontend.Components.RenderProofTab (
  renderProofTab
) where

import Prelude hiding (span)

import Frontend.Types
import Frontend.Themes (getActualTheme)
import Frontend.Components.GeneralUIComponents
import Frontend.SpecialCharacters (replaceSpecialSymbols, replaceSpecialSymbolsInverse)
import Frontend.Helper.General (trimText, extractErrorMsg, getWarningsInSubProof, isErrorSubProof, isErrorLine, getWarningsOnLine, evalPath)
import Frontend.Parse (validateRuleArgument, parseRule, validateStatement, validateRule)
import Shared.Messages
import Logic.Par (myLexer, pForm)

import Monomer
import Monomer.Widgets.Singles.Base.InputField (InputFieldState (_ifsCurrText, _ifsCursorPos))
import qualified Monomer.Lens as L
import Data.Text (Text, pack, intercalate, unpack, toLower, isInfixOf)
import qualified Data.Text (length)
import Data.Default (Default(def))
import Data.Either (isLeft)
import qualified Data.Map
import Control.Lens
import TextShow (showt)

import Monomer.Widgets.Containers.TextFieldSuggestions
import Data.Set (fromList, toList)

import Control.Concurrent.Timer
import Control.Concurrent.Suspend
import Control.Concurrent (threadDelay)

renderProofTab
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> File
  -> Text
  -> WidgetNode AppModel AppEvent
renderProofTab _wenv model file heading = renderProofTab' file heading where
  selTheme = getActualTheme $ model ^. preferences . selectedTheme
  -- accentColor = selTheme ^. L.userColorMap . at "accent" . non def
  -- popupBackground = selTheme ^. L.userColorMap . at "popupBackground" . non def
  -- backgroundColor = selTheme ^. L.userColorMap . at "backgroundColor" . non def
  -- selectedColor = selTheme ^. L.userColorMap . at "selectedFileBg" . non def
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
  -- iconLabel = Frontend.Components.GeneralUIComponents.iconLabel model
  -- iconButton = Frontend.Components.GeneralUIComponents.iconButton model
  trashButton = Frontend.Components.GeneralUIComponents.trashButton model
  -- bold = Frontend.Components.GeneralUIComponents.bold model
  -- normalStyle = Frontend.Components.GeneralUIComponents.normalStyle model
  symbolStyle = Frontend.Components.GeneralUIComponents.symbolStyle model

  button = Frontend.Components.GeneralUIComponents.button model
  fastTooltip = Frontend.Components.GeneralUIComponents.fastTooltip model
  -- fastScroll = Frontend.Components.GeneralUIComponents.fastScroll
  fastVScroll = Frontend.Components.GeneralUIComponents.fastVScroll
  -- fastHScroll = Frontend.Components.GeneralUIComponents.fastHScroll

  renderProofTab' :: File -> Text -> WidgetNode AppModel AppEvent
  renderProofTab' file heading = case parsedSequent of
    Nothing -> vstack [
        vstack [
          h1 $ pack $ _path file,
          spacer
        ] `styleBasic` [padding 10, borderB 1 dividerColor],
        fastVScroll (vstack [
          paragraph "Corrupt proof! Try editing the proof-file in a text editor to fix it. Close this tab and reopen the proof-file after corrupted data is removed",
          spacer,
          paragraph "File preview:",
          spacer,
          symbolSpan_ (_content file) [multiline]
        ]) `styleBasic` [padding 10]
      ]
    Just parsedSequent -> vstack [
        vstack [
          h1 heading,
          spacer,
          subheading
        ] `styleBasic` [padding 10, borderB 1 dividerColor],
        fastVScroll (proofTreeUI parsedSequent) `styleBasic` [padding 10],
        hstack [
          proofStatusLabel,
          filler,
          button "Save proof" (SaveFile file),
          spacer,
          button "Check proof" (CheckProof file)
          -- button "Export to LaTeX" ExportToLaTeX 
          --   `styleBasic` [padding 5]
          --   `nodeKey` "ExportSuccess"
          --   `nodeKey` "ExportError",
          -- spacer
        ] `styleBasic` [padding 10, borderT 1 dividerColor]
      ]
      where
        subheading
          | null premises && conclusion == "" = span "Empty proof"
          | otherwise = symbolSpan prettySequent
        prettySequent = intercalate ", " premises <> " ⊢ " <> conclusion
        conclusion = replaceSpecialSymbols (_conclusion parsedSequent)
        premises = map replaceSpecialSymbols (_premises parsedSequent)
    where
      parsedSequent = _parsedSequent file

  proofStatusLabel = case model ^. proofStatus of
    Nothing -> span "Checking proof..." `styleBasic` [textColor orange]
    Just (FEError _warns error) -> paragraph ("Proof is incorrect: " <> (pack . show) error) `styleBasic` [textColor red]
    Just (FEOk warns) ->
        if null warns
        then span "Proof is correct :)" `styleBasic` [textColor lime]
        else vstack [
            span "Proof is correct, but there are warnings:" `styleBasic` [textColor orange],
            vstack (map (flip styleBasic [textColor orange] . span . pack . show) warns)
        ]

  proofTreeUI :: FESequent -> WidgetNode AppModel AppEvent
  proofTreeUI sequent = vstack [
      vstack_ [childSpacing] [
        h2 "Premises",
        vstack_ [childSpacing] $ zipWith premiseLine (_premises sequent) [0..],
        widgetIf (null $ _premises sequent) (span "No premises")
      ],
      spacer,
      hstack [button "+ Premise" (AddPremise (nrPremises - 1))],
      spacer, spacer,

      vstack_ [childSpacing] [
        h2 "Conclusion",
        spacer,
        box_ [alignLeft] (
          firstKeystroke [
            ("Up", FocusOnKey $ WidgetKey ("premise.input." <> showt (nrPremises - 1)), nrPremises >= 1),
            ("Down", NextFocus 1, True),
            ("Enter", NextFocus 1, True)
          ] (symbolStyle $ textFieldV_ (replaceSpecialSymbols (_conclusion sequent)) EditConclusion [placeholder "Enter conclusion here"]
            `styleBasic` [maxWidth 600]
            `styleBasic` [styleIf isConclusionError (border 1 red)]
            `nodeKey` "conclusion.input")
        )
      ],
      spacer, spacer,

      h2 "Proof",
      spacer,
      hstack [
        lineNumbers,
        tree
      ],

      -- Hack so last proof line can scroll all the way to the top
      box (label "") `styleBasic` [height 1000]
    ]
    where
      isConclusionError = isLeft (pForm (myLexer (unpack (replaceSpecialSymbolsInverse (_conclusion sequent)))))
      nrPremises = length (_premises sequent)
      premiseLine premise idx = box_ [alignLeft] (hstack [
          firstKeystroke [
            ("Up", FocusOnKey $ WidgetKey ("premise.input." <> showt (idx - 1)), True),
            ("Down", FocusOnKey $ WidgetKey ("premise.input." <> showt (idx + 1)), idx < nrPremises - 1),
            ("Down", FocusOnKey $ WidgetKey "conclusion.input", idx >= nrPremises - 1),
            ("Delete", RemovePremise idx, True),
            ("Ctrl-Enter", AddPremise idx, True)
          ] (symbolStyle $ textFieldV_ (replaceSpecialSymbols premise) (EditPremise idx) [placeholder "Enter premise"]
            `nodeKey` ("premise.input." <> showt idx)
            `styleBasic` [styleIf isPremiseError (border 1 red)]),
          spacer,
          fastTooltip "Remove line" $ trashButton (RemovePremise idx),
          spacer
        ] `styleBasic` [maxWidth 400]) `nodeKey` ("premise.line." <> showt idx)
        where isPremiseError = trimText premise == "" || isLeft (pForm (myLexer (unpack (replaceSpecialSymbolsInverse premise))))

      -- pfDropTarget idx w = dropTarget_ ((\msg -> Print (show msg <> " to " <> show idx)) :: FormulaPath -> AppEvent) [dropTargetStyle hoverStyle] w
      pfDropTarget idx w = dropTarget_ ((\p -> MovePathToPath p idx) :: FormulaPath -> AppEvent) [dropTargetStyle hoverStyle] w
        where hoverStyle = [border 3 red, bgColor hoverColor]

      tree = vstack [
          ui,
          spacer,
          hstack_ [childSpacing] [
            button "+ New line" AddLine,
            button "+☐ New sub proof" AddSubProof
          ]
        ]
        where
          ui = vstack_ [childSpacing] (ghostPremises ++ map fst s)
          s = getSubProof (_steps sequent) [] 0 1

          ghostPremises = map ghostPremise (_premises sequent)
          ghostPremise premise = hstack [
              symbolSpan pp,
              filler,
              symbolSpan "premise" `styleBasic` [width 300, paddingH 10],
              spacer,
              vstack [] `styleBasic` [width 250]
            ] `styleBasic` [height 34]
            where pp = replaceSpecialSymbols premise

      pf :: FEStep -> Integer -> FormulaPath -> (WidgetNode AppModel AppEvent, Integer)
      pf (SubProof p) index path = (ui, lastIndex)
        where
          ui = draggable_ path [transparency 0.3] $ vstack [
              hstack [
                pfDropTarget path (vstack [] `styleBasic` [width 24]),

                vstack_ [childSpacing] (map fst s)
              ]
                `styleBasic` [border 1 proofBoxColor, styleIf isWarning (border 1 orange), styleIf isError (border 1 red), borderR 0 transparent, paddingV 8],

              widgetIf isError ((paragraph . pack . extractErrorMsg) (model ^. proofStatus))
                `styleBasic` [textColor red, paddingT (0.5*u)],

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
          ui = pfDropTarget path $ draggable_ path [transparency 0.3] $ box_ [onBtnReleased handleBtn, expandContent] $ vstack [
              hstack [
                hstack [
                  -- span (pack (show (pForm (myLexer (unpack (replaceSpecialSymbolsInverse statement)))))),
                  -- span (replaceSpecialSymbolsInverse statement),

                  -- symbolSpan (showt index <> ".") `styleBasic` [textFont $ fromString $ model ^. logicFont],
                  -- spacer,

                  -- textFieldV "" (\_ -> NoEvent),

                  firstKeystroke [
                    ("Up", FocusOnKey $ WidgetKey (showt (index - 1) <> ".statement"), prevIndexExists),
                    ("Up", FocusOnKey $ WidgetKey "conclusion.input", not prevIndexExists),
                    ("Down", FocusOnKey $ WidgetKey (showt (index + 1) <> ".statement"), nextIndexExists),
                    -- ("Right", FocusOnKey $ WidgetKey (showt index <> ".rule"), True),

                    ("Ctrl-Tab", SwitchLineToSubProof path (WidgetKey $ showt index <> ".statement"), True),
                    ("Ctrl-Shift-Tab", SwitchSubProofToLine pathToParentSubProof (WidgetKey $ showt index <> ".statement"), True),
                    ("Delete", RemoveLine False path, trashActive),
                    ("Backspace", RemoveLine False path, canBackspaceToDelete),
                    ("Ctrl-Enter", InsertLineAfter False path, not isLastLine || not nextIndexExists),
                    ("Ctrl-Enter", InsertLineAfter False pathToParentSubProof, isLastLine),
                    ("Enter", NextFocus 1, True)
                  ] (symbolStyle $ textFieldV_ (replaceSpecialSymbols statement) (EditFormula path) 
                      [onKeyDown handleFormulaKey, placeholder "Empty statement", onChange handleAutoCheckProof]
                    `styleBasic` [styleIf isWarning (border 1 orange)]
                    `styleBasic` [styleIf isStatementError (border 1 red)]
                    `nodeKey` (showt index <> ".statement"))
                      `nodeKey` (showt index <> ".statement.keystroke"),

                  spacer,

                  hstack_ [childSpacing] [
                    -- ruleKeystrokes ruleField

                    ruleKeystrokes $ textFieldSuggestionsV (replaceSpecialSymbols rule) (\_i t -> EditRuleName path t) allRules (const ruleField) label
                      `styleBasic` [styleIf isWarning (border 1 orange)]
                      `styleBasic` [styleIf isRuleError (border 1 red)],

                    argInputs
                  ]
                    `styleBasic` [width 300]
                ],
                spacer,
                b
              ],

              widgetIf isError ((span . pack . extractErrorMsg) (model ^. proofStatus))
                `styleBasic` [textColor red, paddingT (0.5*u)],

              vstack (map warningLabel warnings)
            ]
              `nodeKey` showt index

          ruleKeystrokes w = firstKeystroke [
              ("Up", FocusOnKey $ WidgetKey (showt (index - 1) <> ".rule"), prevIndexExists),
              ("Up", FocusOnKey $ WidgetKey "conclusion.input", not prevIndexExists),
              ("Down", FocusOnKey $ WidgetKey (showt (index + 1) <> ".rule"), nextIndexExists),
              -- ("Left", FocusOnKey $ WidgetKey (showt index <> ".statement"), True),

              ("Ctrl-Tab", SwitchLineToSubProof path (WidgetKey $ showt index <> ".rule"), True),
              ("Ctrl-Shift-Tab", SwitchSubProofToLine pathToParentSubProof (WidgetKey $ showt index <> ".rule"), True),
              ("Delete", RemoveLine False path, trashActive),
              ("Backspace", FocusOnKey (WidgetKey (showt index <> ".statement")), rule == ""),
              ("Ctrl-Enter", InsertLineAfter False path, not isLastLine || not nextIndexExists),
              ("Ctrl-Enter", InsertLineAfter False pathToParentSubProof, isLastLine),
              ("Enter", NextFocus 1, usedArguments > 0),
              ("Enter", InsertLineAfter False path, usedArguments == 0)
              -- ("Enter", InsertLineAfter path, True)
            ] w
              `nodeKey` (showt index <> ".rule.keystroke")

          ruleField = symbolStyle $ textFieldV_ (replaceSpecialSymbols rule) (EditRuleName path) [onKeyDown handleRuleNameKey, placeholder "No rule", selectOnFocus]
              `styleBasic` [styleIf isWarning (border 1 orange)]
              `styleBasic` [styleIf isRuleError (border 1 red)]
              `nodeKey` (showt index <> ".rule")

          argInputs = widgetIf (usedArguments /= 0) $ hstack_ [childSpacing] (zipWith argInput (take usedArguments arguments) [0..])
          argInput argument idx = hstack [
              symbolSpan (labels !! idx),
              firstKeystroke [
                ("Up", FocusOnKey $ WidgetKey (showt (index - 1) <> ".ruleArg." <> showt idx), prevIndexExists),
                ("Up", FocusOnKey $ WidgetKey "conclusion.input", not prevIndexExists),
                ("Down", FocusOnKey $ WidgetKey (showt (index + 1) <> ".ruleArg." <> showt idx), nextIndexExists),
                ("Ctrl-Tab", SwitchLineToSubProof path (WidgetKey $ showt index <> ".ruleArg." <> showt idx), True),
                ("Ctrl-Shift-Tab", SwitchSubProofToLine pathToParentSubProof (WidgetKey $ showt index <> ".ruleArg." <> showt idx), True),
                ("Delete", RemoveLine False path, trashActive),
                ("Backspace", FocusOnKey (WidgetKey (showt index <> ".rule")), isFirstArg && argument == ""),
                ("Backspace", FocusOnKey (WidgetKey (showt index <> ".ruleArg." <> showt (idx - 1))), not isFirstArg && argument == ""),
                ("Ctrl-Enter", InsertLineAfter False path, not isLastLine || not nextIndexExists),
                ("Ctrl-Enter", InsertLineAfter False pathToParentSubProof, isLastArg && isLastLine),
                ("Enter", InsertLineAfter False path, isLastArg),
                ("Enter", NextFocus 1, not isLastArg)
              ] (symbolStyle $ textFieldV_ (replaceSpecialSymbols argument) (EditRuleArgument path idx) [onKeyDown (handleRuleArgKey idx), placeholder ("Arg. " <> showt (index + 1)), selectOnFocus]
              `nodeKey` (showt index <> ".ruleArg." <> showt idx)
              `styleBasic` [width 70]
              `styleBasic` [styleIf isWarning (border 1 orange)]
              `styleBasic` [styleIf isRuleArgError (border 1 red)])
            ]
            where
              isFirstArg = idx == 0
              isLastArg = idx + 1 == usedArguments
              isRuleArgError = isError || not (validateRuleArgument argument)
              labels = case Data.Map.lookup (parseRule rule) ruleMetaDataMap of
                Nothing -> repeat ""
                Just (RuleMetaData _ l) -> l

          b = box $ hstack_ [childSpacing] [
                fastTooltip "Remove line" $
                  trashButton (RemoveLine False path)
                    `nodeEnabled` trashActive,

                vstack [
                  fastTooltip "Insert line above" $
                    button "↑+" (InsertLineBefore False path) `styleBasic` [textSize (0.75*u), width 50, height 17, padding 0, radiusBL 0, radiusBR 0],
                  fastTooltip "Insert line below" $
                    button "↓+" (InsertLineAfter False path) `styleBasic` [textSize (0.75*u), width 50, height 17, padding 0, radiusTL 0, radiusTR 0, borderT 1 dividerColor]
                ] `styleBasic` [height 34],

                -- fastTooltip "Insert subproof below" $
                --   button "↓☐+" (InsertSubProofAfter path),

                fastTooltip "Convert line to subproof" $
                  button "→☐" (SwitchLineToSubProof path (WidgetKey $ showt index <> ".statement")),

                widgetIf isSubProofSingleton $
                  fastTooltip "Undo subproof" $
                    button "☒" (SwitchSubProofToLine pathToParentSubProof (WidgetKey $ showt index <> ".statement")),

                widgetIf (isLastLine && nextIndexExists) $
                  fastTooltip "Close subproof" $
                    button "⏎" (InsertLineAfter False pathToParentSubProof)

                -- widgetIf isLastLine (button "/[]+" (InsertSubProofAfter pathToParentSubProof))
              ] `styleBasic` [width 250]

          -- allRules = map fst visualRuleNames
          allRules = (toList . fromList) $ filter (\f -> (toLower . replaceSpecialSymbols) rule `isInfixOf` toLower f) ((map fst visualRuleNames) ++ (map snd visualRuleNames))
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
          
          handleAutoCheckProof :: Text -> AppEvent
          handleAutoCheckProof text = AutoCheckProof

          isStatementError = isError || not (validateStatement statement)
          isRuleError = isError || not (validateRule rule)
          isError = isErrorLine lineNumber (model ^. proofStatus)
          isWarning = not (null warnings)
          warnings = getWarningsOnLine lineNumber (model ^. proofStatus)
          lineNumber = index + toInteger (length (_premises sequent))
          canBackspaceToDelete = rule == "" && statement == "" && trashActive
          trashActive = not (index == 1 && not nextIndexExists && statement == "" && rule == "")
          pathToParentSubProof = init path
          lastIndex = index + 1
          prevIndexExists = index > 1
          nextIndexExists = not (isLastLine && length path == 1)
          isSubProofSingleton = length path /= 1 && isSingleton (evalPath pathToParentSubProof sequent)
          isSingleton (SubProof p) = length p == 1
          isSingleton _ = False
          isLastLine = case evalPath pathToParentSubProof sequent of
            SubProof p -> length p == last path + 1
            _ -> False

      warningLabel w = span (pack w) `styleBasic` [textColor orange, paddingT (0.5*u)]

      getSubProof p path arrayIndex visualIndex
        | arrayIndex < length p = u : getSubProof p path (arrayIndex + 1) (snd u)
        | otherwise = []
          where u = pf (p !! arrayIndex) visualIndex (path ++ [arrayIndex])

      autoCheckingProof :: IO AppEvent
      autoCheckingProof = do
        threadDelay 1048576
        yn <- return (model ^. autoCheckProofTracker . autoCheckProofIf)
        case yn of
          False -> do 
            _ <- return NoEvent
            autoCheckingProof
          True -> do
            _ <- return (CheckProof file)
            _ <- return (SetAutoCheckProofIf False)
            autoCheckingProof

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