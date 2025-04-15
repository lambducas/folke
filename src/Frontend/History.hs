{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.History (
  saveModelToHistory
) where

import Frontend.Types

import Control.Lens
import Data.List (find)

saveModelToHistory :: AppModel -> AppModel -> AppModel
saveModelToHistory oldModel newModel =
  if oldModel ^. ignoreHistoryOnce
  then newModel & ignoreHistoryOnce .~ False
  else
    let shouldSave = hasChanged oldModel newModel
        
        truncatedHistory = take (oldModel ^. historyIndex) (oldModel ^. stateHistory)
        newHistory = if shouldSave
                     then take 50 (oldModel : truncatedHistory)
                     else oldModel ^. stateHistory
    in newModel
         & stateHistory .~ newHistory
         & historyIndex .~ (if shouldSave then length newHistory else oldModel ^. historyIndex)

hasChanged :: AppModel -> AppModel -> Bool
hasChanged oldModel newModel =
  let oldFiles = oldModel ^. persistentState . tmpLoadedFiles
      newFiles = newModel ^. persistentState . tmpLoadedFiles
      
      currentPath = oldModel ^. persistentState . currentFile
      
      filesChanged = case currentPath of
        Nothing -> False
        Just path -> 
          let oldFile = getProofFileByPath oldFiles path
              newFile = getProofFileByPath newFiles path
          in case (oldFile, newFile) of
               (Just o, Just n) -> 
                 _parsedSequent o /= _parsedSequent n
               _ -> False
  in filesChanged

getProofFileByPath :: [File] -> FilePath -> Maybe File
getProofFileByPath files path = find (\f -> _path f == path) files
