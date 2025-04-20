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
import Data.Text (Text)
import Data.List (groupBy)
import Frontend.Components.GeneralUIComponents ( iconLabel, span_ )
import Monomer
import Frontend.Types
import Frontend.Themes ( getActualTheme )
import System.FilePath.Posix ((</>), takeExtension)

newtype DetailsModel = DetailsModel {
  _dOpen :: Bool
} deriving (Show, Eq)

instance Default DetailsModel where
  def = DetailsModel {
    _dOpen = False
  }

makeLensesWith abbreviatedFields 'DetailsModel

data DetailsEvt
  = DNoEvent
  | DToggleOpen
  | DOpenFile FilePath
  | DOpenContextMenu FilePath FilePath
  deriving (Eq, Show)

type Parts = [([Text], String)]

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
  -> Parts
  -> WidgetNode AppModel AppEvent
details configs appModel parts = newNode where
  config = mconcat configs
  model = WidgetValue def
  uiBuilder = buildUI appModel parts 1
  eventHandler =  handleEvent config
  mergeModel _wenv _parentModel oldModel _newModel = oldModel
  compCfg = [compositeMergeModel mergeModel]
  newNode = compositeD_ "details" model uiBuilder eventHandler compCfg

details2
  :: [DetailsCfg DetailsModel DetailsEvt]
  -> AppModel
  -> Text
  -> Parts
  -> Double
  -> WidgetNode DetailsModel DetailsEvt
details2 configs appModel header parts indent = newNode where
  config = mconcat configs
  model = WidgetValue def
  uiBuilder = buildUI2 configs appModel header parts indent
  eventHandler = handleEvent config
  mergeModel _wenv _parentModel oldModel _newModel = oldModel
  compCfg = [compositeMergeModel mergeModel]
  newNode = compositeD_ "details2" model uiBuilder eventHandler compCfg

buildUI
  :: AppModel
  -> Parts
  -> Double
  -> WidgetEnv DetailsModel DetailsEvt
  -> DetailsModel
  -> WidgetNode DetailsModel DetailsEvt
buildUI appModel parts indent _wenv _model = widgetTree where
  selTheme = getActualTheme $ appModel ^. preferences . selectedTheme
  selectedColor = selTheme ^. L.userColorMap . at "selectedFileBg" . non def
  dividerColor = selTheme ^. L.userColorMap . at "dividerColor" . non def
  hoverColor = selTheme ^. L.userColorMap . at "hoverColor" . non def

  widgetTree = fileTreeUI parts indent

  fileTreeUI parts indent = vstack [
      vstack $ map folder groups,
      vstack $ map (\f -> fileItem indent (fst f) (snd f)) partFile
    ]
    where
      -- parts = map (\f -> (splitOn "/" (pack f), f)) files
      partFile = map (\f -> ((head . fst) f, snd f)) (filter (\i -> length (fst i) == 1) parts)
      partFolder = filter (\i -> length (fst i) > 1) parts
      groups = groupBy (\a b -> head (fst a) == head (fst b)) partFolder

      -- oldCfg = map (\func fp -> RaiseEvent $ ) $ _dcOnOpenFile cfg
      newCfg = DetailsCfg {
        _dcOnOpenFile = [RaiseEvent . DOpenFile],
        _dcOnOpenContextMenu = [\a b -> RaiseEvent $ DOpenContextMenu a b]
      }

      folder seqs = details2 [newCfg] appModel heading newParts (indent + 1)
        where
          heading = (head . fst . head) seqs
          newParts = map (\f -> ((tail . fst) f, snd f)) seqs

  fileItem indent text filePath = box_ [expandContent, onBtnReleased handleBtn] $ hstack_ [childSpacing] [
      iconLabel appModel iconIdent `styleBasic` [fromMaybe mempty (iconColor >>= Just . textColor)],
      span_ appModel text [ellipsis]
    ]
      `styleHover` [styleIf (not isCurrent) (bgColor hoverColor)]
      `styleBasic` [borderB 1 dividerColor, paddingL (16 * indent), paddingR 16, paddingV 8, cursorHand, styleIf isCurrent (bgColor selectedColor)]
    where
      handleBtn BtnLeft _ = DOpenFile filePath
      handleBtn BtnRight _ = DOpenContextMenu fullPath filePath
      handleBtn _ _ = DNoEvent
      isCurrent = (appModel ^. persistentState . currentFile) == Just fullPath
      fullPath = case appModel ^. persistentState . workingDir of
        Nothing -> ""
        Just wd -> wd </> filePath
      ext = takeExtension filePath
      iconIdent
        | ext == ".md" = remixMarkdownFill
        | ext == "." <> feFileExt = remixBracesFill --remixSurveyFill
        | otherwise = remixMenu2Line
      iconColor
        | ext == ".md" = Just $ rgb 94 156 255
        | ext == "." <> feFileExt = Just $ rgb 255 130 0 --Just $ rgb 255 130 0
        | otherwise = Nothing

buildUI2
  :: [DetailsCfg DetailsModel DetailsEvt]
  -> AppModel
  -> Text
  -> Parts
  -> Double
  -> WidgetEnv DetailsModel DetailsEvt
  -> DetailsModel
  -> WidgetNode DetailsModel DetailsEvt
buildUI2 cfg appModel headerText parts indent _wenv model = widgetTree where
  selTheme = getActualTheme $ appModel ^. preferences . selectedTheme
  selectedColor = selTheme ^. L.userColorMap . at "selectedFileBg" . non def
  dividerColor = selTheme ^. L.userColorMap . at "dividerColor" . non def
  hoverColor = selTheme ^. L.userColorMap . at "hoverColor" . non def

  widgetTree = vstack [
      box_ [onClick DToggleOpen, expandContent] header,
      fileTreeUI parts indent `nodeVisible` _dOpen model
    ]

  header = hstack [
      iconLabel appModel remixFolder5Line `styleBasic` [paddingR 8],
      label headerText
    ] `styleBasic` [paddingL (16 * (indent - 1)), paddingV 8, cursorHand]

  fileTreeUI parts indent = vstack [
      vstack $ map folder groups,
      vstack $ map (\f -> fileItem indent (fst f) (snd f)) partFile
    ]
    where
      -- parts = map (\f -> (splitOn "/" (pack f), f)) files
      partFile = map (\f -> ((head . fst) f, snd f)) (filter (\i -> length (fst i) == 1) parts)
      partFolder = filter (\i -> length (fst i) > 1) parts
      groups = groupBy (\a b -> head (fst a) == head (fst b)) partFolder

      folder seqs = details2 cfg appModel heading newParts (indent + 1)
        where
          heading = (head . fst . head) seqs
          newParts = map (\f -> ((tail . fst) f, snd f)) seqs

  fileItem indent text filePath = box_ [expandContent, onBtnReleased handleBtn] $ hstack_ [childSpacing] [
      iconLabel appModel iconIdent `styleBasic` [fromMaybe mempty (iconColor >>= Just . textColor)],
      span_ appModel text [ellipsis]
    ]
      `styleHover` [styleIf (not isCurrent) (bgColor hoverColor)]
      `styleBasic` [borderB 1 dividerColor, paddingL (16 * indent), paddingR 16, paddingV 8, cursorHand, styleIf isCurrent (bgColor selectedColor)]
    where
      handleBtn BtnLeft _ = DOpenFile filePath
      handleBtn BtnRight _ = DOpenContextMenu fullPath filePath
      handleBtn _ _ = DNoEvent
      isCurrent = (appModel ^. persistentState . currentFile) == Just fullPath
      fullPath = case appModel ^. persistentState . workingDir of
        Nothing -> ""
        Just wd -> wd </> filePath
      ext = takeExtension filePath
      iconIdent
        | ext == ".md" = remixMarkdownFill
        | ext == "." <> feFileExt = remixBracesFill --remixSurveyFill
        | otherwise = remixMenu2Line
      iconColor
        | ext == ".md" = Just $ rgb 94 156 255
        | ext == "." <> feFileExt = Just $ rgb 255 130 0 --Just $ rgb 255 130 0
        | otherwise = Nothing

handleEvent
  :: WidgetModel sp
  => DetailsCfg sp ep
  -> WidgetEnv DetailsModel DetailsEvt
  -> WidgetNode DetailsModel DetailsEvt
  -> DetailsModel
  -> DetailsEvt
  -> [EventResponse DetailsModel DetailsEvt sp ep]
handleEvent cfg _wenv _node model evt = case evt of
  DNoEvent -> []
  DOpenFile filePath -> reportOpenFile filePath
  DOpenContextMenu fullPath filePath -> reportOpenContextMenu fullPath filePath
  DToggleOpen -> [ Model $ model & open %~ not ]
  where
    report reqs = RequestParent <$> reqs
    reportOpenFile filePath = report (($ filePath) <$> _dcOnOpenFile cfg)
    reportOpenContextMenu fullPath filePath = report (($ filePath) . ($ fullPath) <$> _dcOnOpenContextMenu cfg)