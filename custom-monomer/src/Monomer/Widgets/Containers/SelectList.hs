{-|
Module      : Monomer.Widgets.Containers.SelectList
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Select list widget, allowing selection of a single item. List content (rows) is
customizable, and so is its styling. This widget is used by
"Monomer.Widgets.Containers.Dropdown" when in its open state.

@
makeRow username = hstack [
    label "User: ",
    label username
  ]

customSelect = selectList userLens usernames makeRow
@

Note: the content of the list will only be updated when the provided items
change, based on their 'Eq' instance. In case data external to the items is used
for building the row nodes, 'mergeRequired' may be needed to avoid stale
content.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.Containers.SelectList (
  -- * Configuration
  SelectListCfg,
  SelectListItem,
  SelectListMessage(..),
  SelectListMakeRow,
  -- * Constructors
  selectList,
  selectList_,
  selectListV,
  selectListV_,
  selectListD_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~), (%~), at, ix)
import Data.Default
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (|>))
import Data.Typeable (Typeable, Proxy, cast, typeRep)
import TextShow

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container
import Monomer.Widgets.Containers.Box
import Monomer.Widgets.Containers.Scroll
import Monomer.Widgets.Containers.Stack

import qualified Monomer.Lens as L

-- | Constraints for an item handled by selectList.
type SelectListItem a = (Eq a, Show a, Typeable a)
-- | Creates a row from an item.
type SelectListMakeRow s e a = a -> WidgetNode s e

{-|
Configuration options for selectList:

- 'onFocus': event to raise when focus is received.
- 'onFocusReq': 'WidgetRequest' to generate when focus is received.
- 'onBlur': event to raise when focus is lost.
- 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
- 'onChange': event to raise when selected item changes.
- 'onChangeReq': 'WidgetRequest' to generate when selected item changes.
- 'onChangeIdx': event to raise when selected item changes. Includes index.
- 'onChangeIdxReq': 'WidgetRequest' to generate when selected item changes.
  Includes index.
- 'selectOnBlur': whether to select the currently highlighted item when
  navigating away from the widget with tab key.
- 'itemBasicStyle': style of an item in the list when not selected.
- 'itemSelectedStyle': style of the selected item in the list.
- 'mergeRequired': whether merging children is required. Useful when the content
  displayed depends on external data, since changes to data outside the provided
  list cannot be detected. In general it is recommended to only depend on data
  contained in the list itself, making sure the 'Eq' instance of the item type
  is correct.
-}
data SelectListCfg s e a = SelectListCfg {
  _slcSelectOnBlur :: Maybe Bool,
  _slcItemStyle :: Maybe Style,
  _slcItemSelectedStyle :: Maybe Style,
  _slcMergeRequired :: Maybe (WidgetEnv s e -> Seq a -> Seq a -> Bool),
  _slcOnFocusReq :: [Path -> WidgetRequest s e],
  _slcOnBlurReq :: [Path -> WidgetRequest s e],
  _slcOnChangeReq :: [a -> WidgetRequest s e],
  _slcOnChangeIdxReq :: [Int -> a -> WidgetRequest s e]
}

instance Default (SelectListCfg s e a) where
  def = SelectListCfg {
    _slcSelectOnBlur = Nothing,
    _slcItemStyle = Nothing,
    _slcItemSelectedStyle = Nothing,
    _slcMergeRequired = Nothing,
    _slcOnFocusReq = [],
    _slcOnBlurReq = [],
    _slcOnChangeReq = [],
    _slcOnChangeIdxReq = []
  }

instance Semigroup (SelectListCfg s e a) where
  (<>) t1 t2 = SelectListCfg {
    _slcSelectOnBlur = _slcSelectOnBlur t2 <|> _slcSelectOnBlur t1,
    _slcItemStyle = _slcItemStyle t2 <|> _slcItemStyle t1,
    _slcItemSelectedStyle = _slcItemSelectedStyle t2 <|> _slcItemSelectedStyle t1,
    _slcMergeRequired = _slcMergeRequired t2 <|> _slcMergeRequired t1,
    _slcOnFocusReq = _slcOnFocusReq t1 <> _slcOnFocusReq t2,
    _slcOnBlurReq = _slcOnBlurReq t1 <> _slcOnBlurReq t2,
    _slcOnChangeReq = _slcOnChangeReq t1 <> _slcOnChangeReq t2,
    _slcOnChangeIdxReq = _slcOnChangeIdxReq t1 <> _slcOnChangeIdxReq t2
  }

instance Monoid (SelectListCfg s e a) where
  mempty = def

instance WidgetEvent e => CmbOnFocus (SelectListCfg s e a) e Path where
  onFocus fn = def {
    _slcOnFocusReq = [RaiseEvent . fn]
  }

instance CmbOnFocusReq (SelectListCfg s e a) s e Path where
  onFocusReq req = def {
    _slcOnFocusReq = [req]
  }

instance WidgetEvent e => CmbOnBlur (SelectListCfg s e a) e Path where
  onBlur fn = def {
    _slcOnBlurReq = [RaiseEvent . fn]
  }

instance CmbOnBlurReq (SelectListCfg s e a) s e Path where
  onBlurReq req = def {
    _slcOnBlurReq = [req]
  }

instance WidgetEvent e => CmbOnChange (SelectListCfg s e a) a e where
  onChange fn = def {
    _slcOnChangeReq = [RaiseEvent . fn]
  }

instance CmbOnChangeReq (SelectListCfg s e a) s e a where
  onChangeReq req = def {
    _slcOnChangeReq = [req]
  }

instance WidgetEvent e => CmbOnChangeIdx (SelectListCfg s e a) e a where
  onChangeIdx fn = def {
    _slcOnChangeIdxReq = [(RaiseEvent .) . fn]
  }

instance CmbOnChangeIdxReq (SelectListCfg s e a) s e a where
  onChangeIdxReq req = def {
    _slcOnChangeIdxReq = [req]
  }

instance CmbSelectOnBlur (SelectListCfg s e a) where
  selectOnBlur_ select = def {
    _slcSelectOnBlur = Just select
  }

instance CmbItemBasicStyle (SelectListCfg s e a) Style where
  itemBasicStyle style = def {
    _slcItemStyle = Just style
  }

instance CmbItemSelectedStyle (SelectListCfg s e a) Style where
  itemSelectedStyle style = def {
    _slcItemSelectedStyle = Just style
  }

instance CmbMergeRequired (SelectListCfg s e a) (WidgetEnv s e) (Seq a) where
  mergeRequired fn = def {
    _slcMergeRequired = Just fn
  }

data SelectListState a = SelectListState {
  _prevItems :: Seq a,
  _slIdx :: Int,
  _hlIdx :: Int
} deriving (Eq, Show)

-- | Messages received by selectList. In general used internally.
data SelectListMessage
  = SelectListClickItem Int
  | SelectListShowSelected
  | SelectListPrev
  | SelectListNext
  | SelectListClickHighlighted
  deriving (Eq, Show)

-- | Creates a select list using the given lens.
selectList
  :: (WidgetModel s, WidgetEvent e, Traversable t, SelectListItem a)
  => ALens' s a               -- ^ The lens into the model.
  -> t a                      -- ^ The list of selectable items.
  -> SelectListMakeRow s e a  -- ^ Function to create the list items.
  -> WidgetNode s e           -- ^ The created select list.
selectList field items makeRow = selectList_ field items makeRow def

-- | Creates a select list using the given lens. Accepts config.
selectList_
  :: (WidgetModel s, WidgetEvent e, Traversable t, SelectListItem a)
  => ALens' s a               -- ^ The lens into the model.
  -> t a                      -- ^ The list of selectable items.
  -> SelectListMakeRow s e a  -- ^ Function to create the list items.
  -> [SelectListCfg s e a]    -- ^ The config options.
  -> WidgetNode s e           -- ^ The created select list.
selectList_ field items makeRow configs = newNode where
  newNode = selectListD_ (WidgetLens field) items makeRow configs

-- | Creates a select list using the given value and 'onChange' event handler.
selectListV
  :: (WidgetModel s, WidgetEvent e, Traversable t, SelectListItem a)
  => a                        -- ^ The current value.
  -> (Int -> a -> e)          -- ^ The event to raise on change.
  -> t a                      -- ^ The list of selectable items.
  -> SelectListMakeRow s e a  -- ^ Function to create the list items.
  -> WidgetNode s e           -- ^ The created select list.
selectListV value handler items makeRow = newNode where
  newNode = selectListV_ value handler items makeRow def

-- | Creates a select list using the given value and 'onChange' event handler.
--   Accepts config.
selectListV_
  :: (WidgetModel s, WidgetEvent e, Traversable t, SelectListItem a)
  => a                        -- ^ The current value.
  -> (Int -> a -> e)          -- ^ The event to raise on change.
  -> t a                      -- ^ The list of selectable items.
  -> SelectListMakeRow s e a  -- ^ Function to create the list items.
  -> [SelectListCfg s e a]    -- ^ The config options.
  -> WidgetNode s e           -- ^ The created select list.
selectListV_ value handler items makeRow configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChangeIdx handler : configs
  newNode = selectListD_ widgetData items makeRow newConfigs

-- | Creates a select list providing a 'WidgetData' instance and config.
selectListD_
  :: forall s e t a . (WidgetModel s, WidgetEvent e, Traversable t, SelectListItem a)
  => WidgetData s a           -- ^ The 'WidgetData' to retrieve the value from.
  -> t a                      -- ^ The list of selectable items.
  -> SelectListMakeRow s e a  -- ^ Function to create the list items.
  -> [SelectListCfg s e a]    -- ^ The config options.
  -> WidgetNode s e           -- ^ The created select list.
selectListD_ widgetData items makeRow configs = makeNode wtype widget where
  config = mconcat configs
  newItems = foldl' (|>) Empty items
  newState = SelectListState newItems (-1) 0
  wtype = WidgetType ("selectList-" <> showt (typeRep (undefined :: Proxy a)))
  widget = makeSelectList widgetData newItems makeRow config newState

makeNode :: WidgetType -> Widget s e -> WidgetNode s e
makeNode wtype widget = scroll_ [scrollStyle L.selectListStyle] childNode where
  childNode = defaultWidgetNode wtype widget
    & L.info . L.focusable .~ True

makeSelectList
  :: (WidgetModel s, WidgetEvent e, SelectListItem a)
  => WidgetData s a
  -> Seq a
  -> SelectListMakeRow s e a
  -> SelectListCfg s e a
  -> SelectListState a
  -> Widget s e
makeSelectList widgetData items makeRow config state = widget where
  widget = createContainer state def {
    containerInit = init,
    containerInitPost = initPost,
    containerMergeChildrenReq = mergeChildrenReq,
    containerMerge = merge,
    containerMergePost = mergePost,
    containerHandleEvent = handleEvent,
    containerHandleMessage = handleMessage
  }

  currentValue wenv = widgetDataGet (_weModel wenv) widgetData

  createSelectListChildren wenv node = children where
    widgetId = node ^. L.info . L.widgetId
    selected = currentValue wenv
    itemsList = makeItemsList wenv items makeRow config widgetId selected
    children = Seq.singleton itemsList

  init wenv node = resultNode newNode where
    selected = currentValue wenv
    newSl = fromMaybe (-1) (Seq.elemIndexL selected items)
    newHl = if newSl < 0 then 0 else newSl
    newState = state {
      _slIdx = newSl,
      _hlIdx = newHl
    }
    newNode = node
      & L.widget .~ makeSelectList widgetData items makeRow config newState
      & L.children .~ createSelectListChildren wenv node

  initPost wenv node newState result = newResult where
    newResult = updateResultStyle wenv config result state newState

  mergeChildrenReq wenv node oldNode oldState = result where
    oldItems = _prevItems oldState
    isReload = isWidgetReload wenv
    mergeRequiredFn = fromMaybe (const (/=)) (_slcMergeRequired config)
    result = isReload || mergeRequiredFn wenv oldItems items

  -- merge wenv node oldNode oldState = resultNode newNode where
  merge wenv node oldNode oldState = resultReqs newNode reqs where
    selected = currentValue wenv
    newSl = fromMaybe (-1) (Seq.elemIndexL selected items)
    itemsChanged = _prevItems oldState /= items
    newHl
      | itemsChanged = 0 -- Reset highlighted item when items change
      | newSl /= _slIdx oldState = newSl
      | otherwise = _hlIdx oldState
    newState = oldState {
      _slIdx = newSl,
      _hlIdx = newHl,
      _prevItems = items
    }
    newNode = node
      & L.widget .~ makeSelectList widgetData items makeRow config newState
      & L.children .~ createSelectListChildren wenv node
    reqs = itemScrollTo wenv newNode newHl

  mergePost wenv node oldNode oldState newState result = newResult where
    newResult = updateResultStyle wenv config result oldState newState

  handleEvent wenv node target evt = case evt of
    -- ButtonAction _ btn BtnPressed _
    --   | btn == wenv ^. L.mainButton -> result where
    --     result = Just $ resultReqs node [SetFocus (node ^. L.info . L.widgetId)]

    Click point _ _
      | outsideVp point -> Just ignoreEvtResult

    Move point
      | outsideVp point -> Just ignoreEvtResult

    Focus prev -> handleFocusChange node prev (_slcOnFocusReq config)

    Blur next -> result where
      tabPressed = wenv ^. L.inputStatus . L.keys . at keyTab == Just KeyPressed
      changeReq = tabPressed && _slcSelectOnBlur config == Just True
      WidgetResult tempNode tempReqs
        | changeReq = selectItem wenv node (_hlIdx state)
        | otherwise = resultNode node
      reqs = tempReqs <> Seq.fromList (($ next) <$> _slcOnBlurReq config)
      result
        | changeReq || not (null reqs) = Just $ WidgetResult tempNode reqs
        | otherwise = Nothing

    KeyAction mode code status
      | isKeyDown code && status == KeyPressed -> Just $ highlightNext wenv node
      | isKeyUp code && status == KeyPressed -> Just $ highlightPrev wenv node
      | isSelectKey code && status == KeyPressed -> resultSelected
      where
        resultSelected = Just $ selectItem wenv node (_hlIdx state)
        isSelectKey code = isKeyReturn code || isKeySpace code
    _ -> Nothing

    where
      outsideVp point = not (pointInRect point (wenv ^. L.viewport))
      ignoreEvtResult = resultReqs node [IgnoreChildrenEvents]

  highlightNext wenv node = highlightItem wenv node nextIdx where
    tempIdx = _hlIdx state
    nextIdx
      | tempIdx < length items - 1 = tempIdx + 1
      | otherwise = tempIdx

  highlightPrev wenv node = highlightItem wenv node nextIdx where
    tempIdx = _hlIdx state
    nextIdx
      | tempIdx > 0 = tempIdx - 1
      | otherwise = tempIdx

  handleMessage wenv node target message = result where
    handleSelect (SelectListClickItem idx) = handleItemClick wenv node idx
    handleSelect SelectListShowSelected = handleItemShow wenv node
    handleSelect SelectListPrev = highlightPrev wenv node
    handleSelect SelectListNext = highlightNext wenv node
    handleSelect SelectListClickHighlighted = handleItemClick wenv node (_hlIdx state)
    result = fmap handleSelect (cast message)

  handleItemClick wenv node idx = result where
    focusReq = SetFocus $ node ^. L.info . L.widgetId
    tempResult = selectItem wenv node idx

    result = tempResult

    -- result
    --   | isNodeFocused wenv node = tempResult
    --   | otherwise = tempResult & L.requests %~ (|> focusReq)

  handleItemShow wenv node = resultReqs node reqs where
    reqs = itemScrollTo wenv node (_slIdx state)

  highlightItem wenv node nextIdx = result where
    newState = state {
      _hlIdx = nextIdx
    }
    tmpNode = node
      & L.widget .~ makeSelectList widgetData items makeRow config newState
    slIdx = _slIdx state

    (newNode, resizeReq) = updateStyles wenv config state tmpNode slIdx nextIdx
    reqs = itemScrollTo wenv newNode nextIdx ++ resizeReq
    result = resultReqs newNode reqs

  selectItem wenv node idx = result where
    selected = currentValue wenv
    value = fromMaybe selected (Seq.lookup idx items)
    valueSetReq = widgetDataSet widgetData value
    scrollToReq = itemScrollTo wenv node idx
    changeReqs = fmap ($ value) (_slcOnChangeReq config)
      ++ fmap (\fn -> fn idx value) (_slcOnChangeIdxReq config)
    (styledNode, resizeReq) = updateStyles wenv config state node idx idx

    newState = state {
      _slIdx = idx,
      _hlIdx = idx
    }
    newNode = styledNode
      & L.widget .~ makeSelectList widgetData items makeRow config newState
    reqs = valueSetReq ++ scrollToReq ++ changeReqs ++ resizeReq
    result = resultReqs newNode reqs

  itemScrollTo wenv node idx = maybeToList (scrollToReq <$> mwid <*> vp) where
    vp = itemViewport node idx
    mwid = widgetIdFromPath wenv (parentPath node)
    scrollToReq wid rect = SendMessage wid (ScrollTo rect)

  itemViewport node idx = viewport where
    lookup idx node = Seq.lookup idx (node ^. L.children)
    viewport = fmap (_wniViewport . _wnInfo) $ pure node
      >>= lookup 0 -- vstack
      >>= lookup idx -- item

updateStyles
  :: WidgetEnv s e
  -> SelectListCfg s e a
  -> SelectListState a
  -> WidgetNode s e
  -> Int
  -> Int
  -> (WidgetNode s e, [WidgetRequest s e])
updateStyles wenv config state node newSlIdx newHlIdx = (newNode, newReqs) where
  widgetId = node ^. L.info . L.widgetId
  items = node ^. L.children . ix 0 . L.children
  normalStyle = getNormalStyle wenv config
  idxMatch = newSlIdx == newHlIdx

  (slStyle, hlStyle)
    | idxMatch = (getSlHlStyle wenv config, getSlHlStyle wenv config)
    | otherwise = (getSlStyle wenv config, getHlStyle wenv config)

  (newChildren, resizeReq) = (items, False)
    & updateItemStyle wenv (_slIdx state) (Just normalStyle)
    & updateItemStyle wenv (_hlIdx state) (Just normalStyle)
    & updateItemStyle wenv newHlIdx (Just slStyle)
    -- & updateItemStyle wenv newHlIdx (Just hlStyle)
    & updateItemStyle wenv newSlIdx (Just slStyle)

  newNode = node
    & L.children . ix 0 . L.children .~ newChildren
  newReqs = [ ResizeWidgets widgetId | resizeReq ]

updateItemStyle
  :: WidgetEnv s e
  -> Int
  -> Maybe Style
  -> (Seq (WidgetNode s e), Bool)
  -> (Seq (WidgetNode s e), Bool)
updateItemStyle wenv idx mstyle (items, resizeReq) = result where
  result = case Seq.lookup idx items of
    Just item -> (newItems, resizeReq || newResizeReq) where
      tmpItem = setItemStyle item mstyle
      (newItem, newResizeReq) = updateItemSizeReq wenv tmpItem
      newItems = Seq.update idx newItem items
    Nothing -> (items, resizeReq)

updateItemSizeReq :: WidgetEnv s e -> WidgetNode s e -> (WidgetNode s e, Bool)
updateItemSizeReq wenv item = (newItem, resizeReq) where
  (oldReqW, oldReqH) = (item^. L.info . L.sizeReqW, item^. L.info . L.sizeReqH)
  (newReqW, newReqH) = widgetGetSizeReq (item ^. L.widget) wenv item
  newItem = item
    & L.info . L.sizeReqW .~ newReqW
    & L.info . L.sizeReqH .~ newReqH
  resizeReq = (oldReqW, oldReqH) /= (newReqW, newReqH)

setItemStyle :: WidgetNode s e -> Maybe Style -> WidgetNode s e
setItemStyle item Nothing = item
setItemStyle item (Just st) = item
  & L.children . ix 0 . L.info . L.style .~ st

getSlStyle :: WidgetEnv s e -> SelectListCfg s e a -> Style
getSlStyle wenv config = style where
  theme = collectTheme wenv L.selectListItemSelectedStyle
  style = fromJust (Just theme <> _slcItemSelectedStyle config)
  slStyle = style
    & L.basic .~ style ^. L.focus
    & L.hover .~ style ^. L.focusHover

getSlHlStyle :: WidgetEnv s e -> SelectListCfg s e a -> Style
getSlHlStyle wenv config = slStyle where
  style = getSlStyle wenv config
  slStyle = style
    & L.basic .~ style ^. L.focus
    & L.hover .~ style ^. L.focusHover

getHlStyle :: WidgetEnv s e -> SelectListCfg s e a -> Style
getHlStyle wenv config = hlStyle where
  theme = collectTheme wenv L.selectListItemStyle
  style = fromJust (Just theme <> _slcItemStyle config)
  hlStyle = style
    & L.basic .~ style ^. L.focus
    & L.hover .~ style ^. L.focusHover

getNormalStyle :: WidgetEnv s e -> SelectListCfg s e a -> Style
getNormalStyle wenv config = style where
  theme = collectTheme wenv L.selectListItemStyle
  style = fromJust (Just theme <> _slcItemStyle config)

updateResultStyle
  :: WidgetEnv s e
  -> SelectListCfg s e a
  -> WidgetResult s e
  -> SelectListState a
  -> SelectListState a
  -> WidgetResult s e
updateResultStyle wenv config result oldState newState = newResult where
  slIdx = _slIdx newState
  hlIdx = _hlIdx newState
  WidgetResult prevNode prevReqs = result

  (newNode, reqs) = updateStyles wenv config oldState prevNode slIdx hlIdx
  newResult = resultNode newNode
    & L.requests .~ prevReqs <> Seq.fromList reqs

makeItemsList
  :: (WidgetModel s, WidgetEvent e, Eq a)
  => WidgetEnv s e
  -> Seq a
  -> SelectListMakeRow s e a
  -> SelectListCfg s e a
  -> WidgetId
  -> a
  -> WidgetNode s e
makeItemsList wenv items makeRow config widgetId selected = itemsList where
  normalTheme = collectTheme wenv L.selectListItemStyle
  normalStyle = fromJust (Just normalTheme <> _slcItemStyle config)

  makeItem idx item = newItem where
    clickCfg = onClickReq $ SendMessage widgetId (SelectListClickItem idx)
    itemCfg = [expandContent, clickCfg]
    content = makeRow item
      & L.info . L.style .~ normalStyle
    newItem = box_ itemCfg content
  itemsList = vstack $ Seq.mapWithIndex makeItem items
