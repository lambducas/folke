{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.History (
  canUndoRedo,
  applyUndo,
  applyRedo
) where

import Frontend.Types
import Frontend.Helper.ProofHelper
import Control.Lens

-- | Check if undo/redo can be applied
canUndoRedo :: AppModel -> Bool
canUndoRedo model = not (model ^. udrPopup) && not (model ^. exportOptionsPopup . eoOpen) 

-- | Checks if file supports undo and undoes the most recent change
applyUndo :: File -> File
applyUndo f@(ProofFile {}) = undo f
applyUndo f@(TemporaryProofFile {}) = undo f
applyUndo f = f

-- | Checks if file supports redo and redoes the most recent undo
applyRedo :: File -> File
applyRedo f@(ProofFile {}) = redo f
applyRedo f@(TemporaryProofFile {}) = redo f
applyRedo f = f

-- | Undoes most recent change on any file (will fail if fail doesn't support undo/redo)
undo :: File -> File
undo f
  | hIdx < 0 || length hSte <= hIdx = f
  | otherwise = newFile
  where
    newFile = f
      & parsedDocument . _Just . sequent %~ applyInverseHistory historyStep
      & history . hIndex %~ \f -> f - 1

    historyStep = hSte !! hIdx
    hSte = _hState (_history f)
    hIdx = _hIndex (_history f)

-- | Redoes most recent undo on any file (will fail if fail doesn't support undo/redo)
redo :: File -> File
redo f
  | hIdx >= length hSte - 1 = f
  | otherwise = newFile
  where
    newFile = f
      & parsedDocument . _Just . sequent %~ applyHistory historyStep
      & history . hIndex %~ (+1)

    historyStep = hSte !! (hIdx + 1)
    hSte = _hState (_history f)
    hIdx = _hIndex (_history f)

-- | Applies the inverse of a history event on a given sequent
applyInverseHistory :: HistoryEvent -> FESequent -> FESequent
applyInverseHistory (HMultiple (e:es)) seq = applyInverseHistory (HMultiple es) (applyInverseHistory e seq)
applyInverseHistory (HMultiple []) seq = seq
applyInverseHistory (HUpdateSequent oldSeq _newSeq) _seq = oldSeq
-- applyInverseHistory (HRemoveStep updateRef path step) seq = insertInProof path step seq
-- applyInverseHistory (HInsertStep updateRef path _step) seq = removeFromProof path True seq
applyInverseHistory (HEditStep path oldStep _newStep) seq = replaceInProof path (const oldStep) seq
-- applyInverseHistory (HMoveStep oldSteps newSteps) seq = replaceSteps (const oldSteps) seq
-- applyInverseHistory (HRemovePremise idx prem) seq = addPremiseToProof idx prem seq
-- applyInverseHistory (HInsertPremise idx _prem) seq = removePremiseFromProof idx seq
applyInverseHistory (HEditPremise idx oldText _newText) seq = editPremisesInProof idx oldText seq
applyInverseHistory (HEditConclusion oldText _newText) seq = editConclusionInProof oldText seq
-- applyInverseHistory _ seq = seq

-- | Applies a history event on a given sequent
applyHistory :: HistoryEvent -> FESequent -> FESequent
applyHistory (HMultiple (e:es)) seq = applyHistory (HMultiple es) (applyHistory e seq)
applyHistory (HMultiple []) seq = seq
applyHistory (HUpdateSequent _oldSeq newSeq) _seq = newSeq
applyHistory (HEditStep path _oldStep newStep) seq = replaceInProof path (const newStep) seq
applyHistory (HEditPremise idx _oldText newText) seq = editPremisesInProof idx newText seq
applyHistory (HEditConclusion _oldText newText) seq = editConclusionInProof newText seq