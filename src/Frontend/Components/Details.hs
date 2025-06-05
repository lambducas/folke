{-|
Component for rendering nested folders with expand/collapse support
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.Components.Details (
  DetailsEvt(..),
  details,
  DetailsCfg(..)
) where

import Control.Lens
import Data.Default ( Default(..) )
import Data.Maybe ( fromMaybe )

import Monomer.Widgets.Container
import Monomer.Widgets.Containers.Stack

import qualified Monomer.Lens as L
import Monomer.Widgets.Composite
import Data.Text (Text, pack)
import Data.List (sort)
import Frontend.Components.GeneralUIComponents ( iconLabel, span_ )
import Monomer
import Frontend.Types
import Frontend.Themes ( getActualTheme )
import System.FilePath ((</>), takeExtension, takeFileName, splitDirectories, makeRelative)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import Control.Monad (filterM)

data DetailsModel = DetailsModel {
  _dOpen :: Bool,
  _dLoadedFiles :: Maybe LoadedFiles,
  _dSelectedFile :: Maybe FilePath
} deriving (Show, Eq)

instance Default DetailsModel where
  def = DetailsModel {
    _dOpen = False,
    _dLoadedFiles = Nothing,
    _dSelectedFile = Nothing
  }

makeLensesWith abbreviatedFields 'LoadedFiles
makeLensesWith abbreviatedFields 'DetailsModel

data DetailsEvt
  = DNoEvent
  | DToggleOpen
  | DLoadedFiles LoadedFiles
  | DOpenFile FilePath
  | DOpenContextMenu FilePath FilePath
  deriving (Eq, Show)

data DetailsCfg s e = DetailsCfg {
  _dcOnOpenFile :: [FilePath -> WidgetRequest s e],
  _dcOnOpenContextMenu :: [FilePath -> FilePath -> WidgetRequest s e]
}

instance Default (DetailsCfg s e) where
  def = DetailsCfg {
    _dcOnOpenFile = [],
    _dcOnOpenContextMenu = []
  }

instance Semigroup (DetailsCfg s e) where
  (<>) a1 a2 = def {
    _dcOnOpenFile = _dcOnOpenFile a1 <> _dcOnOpenFile a2,
    _dcOnOpenContextMenu = _dcOnOpenContextMenu a1 <> _dcOnOpenContextMenu a2
  }

instance Monoid (DetailsCfg s e) where
  mempty = def

details
  :: [DetailsCfg AppModel AppEvent]
  -> AppModel
  -> LoadedFiles
  -> WidgetNode AppModel AppEvent
details configs appModel loadedFiles = newNode where
  config = mconcat configs

  model = WidgetValue $ def {
    _dLoadedFiles = Just loadedFiles
  }

  uiBuilder = buildUI appModel "" 1
  eventHandler =  handleEvent "" config
  mergeModel _wenv _parentModel oldModel _newModel = oldModel {
      _dLoadedFiles = Just loadedFiles,
      _dSelectedFile = _parentModel ^. persistentState . currentFile
    }
  compCfg = [compositeMergeModel mergeModel]
  newNode = compositeD_ "details" model uiBuilder eventHandler compCfg

details2
  :: [DetailsCfg DetailsModel DetailsEvt]
  -> AppModel
  -> FilePath
  -> Text
  -> Double
  -> WidgetNode DetailsModel DetailsEvt
details2 configs appModel parentDirPath header indent = newNode where
  config = mconcat configs
  model = WidgetValue def
  uiBuilder = buildUI2 configs appModel parentDirPath header indent
  eventHandler = handleEvent parentDirPath config
  mergeModel _wenv parentModel oldModel _newModel = oldModel {
      _dSelectedFile = _dSelectedFile parentModel
    }
  compCfg = [compositeMergeModel mergeModel]
  newNode = compositeD_ "details2" model uiBuilder eventHandler compCfg

buildUI
  :: AppModel
  -> FilePath
  -> Double
  -> WidgetEnv DetailsModel DetailsEvt
  -> DetailsModel
  -> WidgetNode DetailsModel DetailsEvt
buildUI appModel parentDirPath indent _wenv model = widgetTree where
  widgetTree = case model ^. loadedFiles of
    Nothing -> label ""
    Just loadedFiles -> vstack [
        vstack $ map folder (loadedFiles ^. directories),
        vstack $ map (fileItem appModel indent) (loadedFiles ^. files)
      ]

  folder directoryPath = details2 newCfg appModel (parentDirPath </> directoryPath) heading (indent + 1)
    where heading = pack . last . splitDirectories $ directoryPath

  newCfg = [DetailsCfg {
    _dcOnOpenFile = [RaiseEvent . DOpenFile],
    _dcOnOpenContextMenu = [\a b -> RaiseEvent $ DOpenContextMenu a b]
  }]

buildUI2
  :: [DetailsCfg DetailsModel DetailsEvt]
  -> AppModel
  -> FilePath
  -> Text
  -> Double
  -> WidgetEnv DetailsModel DetailsEvt
  -> DetailsModel
  -> WidgetNode DetailsModel DetailsEvt
buildUI2 cfg appModel parentDirPath headerText indent _wenv model = widgetTree where
  selTheme = getActualTheme $ appModel ^. preferences . selectedTheme
  hoverColor = selTheme ^. L.userColorMap . at "hoverColor" . non def
  dividerColor = selTheme ^. L.userColorMap . at "dividerColor" . non def

  widgetTree = vstack [
      box_ [onClick DToggleOpen, expandContent] header,
      fileTreeUI `nodeVisible` _dOpen model
    ]

  header = hstack [
      iconLabel appModel remixFolder5Line `styleBasic` [paddingR 8],
      label headerText
    ]
    `styleHover` [bgColor hoverColor]
    `styleBasic` [paddingL (16 * (indent - 1)), paddingV 8, cursorHand]

  fileTreeUI = case model ^. loadedFiles of
    Nothing -> label "Loading files..." `styleBasic` [paddingL (16 * indent), paddingR 16, paddingV 8, textColor dividerColor]
    Just loadedFiles
      | loadedFilesAreEmpty loadedFiles -> label "Empty directory" `styleBasic` [paddingL (16 * indent), paddingR 16, paddingV 8, textColor dividerColor]
      | otherwise -> vstack [
          vstack $ map folder (loadedFiles ^. directories),
          vstack $ map (fileItem appModel indent) (loadedFiles ^. files)
        ]

  folder directoryPath = details2 cfg appModel (parentDirPath </> directoryPath) heading (indent + 1)
    where heading = pack . last . splitDirectories $ directoryPath

handleEvent
  :: WidgetModel sp
  => FilePath
  -> DetailsCfg sp ep
  -> WidgetEnv DetailsModel DetailsEvt
  -> WidgetNode DetailsModel DetailsEvt
  -> DetailsModel
  -> DetailsEvt
  -> [EventResponse DetailsModel DetailsEvt sp ep]
handleEvent parentDirPath cfg _wenv _node model evt = case evt of
  DNoEvent -> []
  DOpenFile filePath -> reportOpenFile filePath
  DOpenContextMenu fullPath filePath -> reportOpenContextMenu fullPath filePath
  DToggleOpen -> case model ^. loadedFiles of
    Nothing -> [ Model $ model & open %~ not, Producer loadFiles ]
    Just _ -> [ Model $ model & open %~ not ]
  DLoadedFiles files -> [ Model $ model & loadedFiles ?~ files ]
  where
    report reqs = RequestParent <$> reqs
    reportOpenFile filePath = report (($ filePath) <$> _dcOnOpenFile cfg)
    reportOpenContextMenu fullPath filePath = report (($ filePath) . ($ fullPath) <$> _dcOnOpenContextMenu cfg)

    loadFiles sendMsg = do
      content <- listDirectory parentDirPath
      onlyFiles <- filterM doesFileExist (map appendTop content)
      onlyDirs <- filterM doesDirectoryExist (map appendTop content)
      sendMsg $ DLoadedFiles LoadedFiles {
        _lFiles = sort onlyFiles,
        _lDirectories = sort onlyDirs
      }

      where
        appendTop :: FilePath -> FilePath
        appendTop = (parentDirPath </>)

fileItem :: AppModel -> Double -> FilePath -> WidgetNode DetailsModel DetailsEvt
fileItem appModel indent filePath = box_ [expandContent, onBtnReleased handleBtn] $ hstack_ [childSpacing] [
    iconLabel appModel iconIdent `styleBasic` [fromMaybe mempty (iconColor >>= Just . textColor)],
    span_ appModel displayLabel [ellipsis]
  ]
    `styleHover` [styleIf (not isCurrent) (bgColor hoverColor)]
    `styleBasic` [styleIf (not isCurrent) (borderB 1 dividerColor), paddingL (16 * indent), paddingR 16, paddingV 8, cursorHand, styleIf isCurrent (bgColor selectedColor)]
  where
    handleBtn BtnLeft _ = DOpenFile relativePath
    handleBtn BtnRight _ = DOpenContextMenu filePath relativePath
    handleBtn _ _ = DNoEvent
    isCurrent = (appModel ^. persistentState . currentFile) == Just filePath
    relativePath = case appModel ^. persistentState . workingDir of
      Nothing -> filePath
      Just wd -> makeRelative wd filePath
    ext = takeExtension filePath
    isProofExt = ext `elem` map ("." <>) feFileExts
    iconIdent
      | ext == ".md" = remixMarkdownFill
      | isProofExt = remixBallPenFill --remixSurveyFill
      | otherwise = remixMenu2Line
    iconColor
      | ext == ".md" = Just $ rgb 94 156 255
      | isProofExt = Just accentColor
      | otherwise = Nothing
    displayLabel = pack (takeFileName filePath)

    selTheme = getActualTheme $ appModel ^. preferences . selectedTheme
    dividerColor = selTheme ^. L.userColorMap . at "dividerColor" . non def
    hoverColor = selTheme ^. L.userColorMap . at "hoverColor" . non def
    selectedColor = selTheme ^. L.userColorMap . at "selectedFileBg" . non def
    accentColor = selTheme ^. L.userColorMap . at "accent" . non def

loadedFilesAreEmpty :: LoadedFiles -> Bool
loadedFilesAreEmpty loadedFiles = null (loadedFiles ^. files) && null (loadedFiles ^. directories)