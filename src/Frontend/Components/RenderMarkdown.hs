{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.Components.RenderMarkdown (
  renderMarkdown
) where

import Prelude hiding (span)

import Frontend.Types
import Frontend.Themes (getActualTheme)
import Frontend.Components.Labels
import CMark
import Monomer
import Data.Text (Text)
import Data.Default ( Default(def) )
import TextShow (showt)
import Control.Lens
import qualified Monomer.Core.Lens as L

renderMarkdown :: AppModel -> Text -> WidgetNode AppModel AppEvent
renderMarkdown model markdown = widgetTree
  where
    widgetTree = convert nodes
    nodes = commonmarkToNode [] markdown

    selTheme = getActualTheme $ model ^. preferences . selectedTheme
    dividerColor = selTheme ^. L.userColorMap . at "dividerColor" . non def
    hoverColor = selTheme ^. L.userColorMap . at "hoverColor" . non def

    convert (Node _ DOCUMENT children) = vstack_ [childSpacing] (map convert children)
    convert (Node _ (HEADING n) [Node _ (TEXT t) _])
      | n == 1 = box_ [alignLeft, expandContent] (h1 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u)]) `styleBasic` [paddingV (0.25*u)]
      | n == 2 = box_ [alignLeft, expandContent] (h2 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u)]) `styleBasic` [paddingV (0.25*u)]
      | n == 3 = box_ [alignLeft, expandContent] (h3 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u)]) `styleBasic` [paddingV (0.25*u)]
      | n == 4 = box_ [alignLeft, expandContent] (h4 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u)]) `styleBasic` [paddingV (0.25*u)]
      | n == 5 = box_ [alignLeft, expandContent] (h5 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u)]) `styleBasic` [paddingV (0.25*u)]
      | n == 6 = box_ [alignLeft, expandContent] (h6 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u)]) `styleBasic` [paddingV (0.25*u)]
      | otherwise = error "Invalid heading level"
    convert (Node _ PARAGRAPH children) = hstack (map convert children)
    convert (Node _ (TEXT t) _) = paragraph model t
    convert (Node _ (CODE t) _) = symbolSpan model t `styleBasic` [bgColor hoverColor, paddingV (0.25*u), paddingH (0.5*u), radius 8]
    convert (Node _ (LIST attr) children) = vstack_ [childSpacing_ (0.5*u)] (zipWith (\f i -> hstack [span model (getBullet i), convert f]) children [startNumber..]) `styleBasic` [paddingL (2*u)]
      where
        getBullet i
          | lt == BULLET_LIST = "• "
          | lt == ORDERED_LIST = showt i <> ". "
          | otherwise = ""
        lt = listType attr
        startNumber = listStart attr
    convert (Node _ ITEM children) = vstack (map convert children)
    convert (Node _ SOFTBREAK _) = paragraph model ""
    convert f = error ("Missed: " ++ show f)