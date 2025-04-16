{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.Components.RenderMarkdown (
  renderMarkdown
) where

import Prelude hiding (span)

import Frontend.Types
import Frontend.Themes (getActualTheme)
import Frontend.Components.GeneralUIComponents
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
      | n == 1 = box_ [alignLeft, expandContent] (h1 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u model)]) `styleBasic` [paddingV (0.25*u model)]
      | n == 2 = box_ [alignLeft, expandContent] (h2 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u model)]) `styleBasic` [paddingV (0.25*u model)]
      | n == 3 = box_ [alignLeft, expandContent] (h3 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u model)]) `styleBasic` [paddingV (0.25*u model)]
      | n == 4 = box_ [alignLeft, expandContent] (h4 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u model)]) `styleBasic` [paddingV (0.25*u model)]
      | n == 5 = box_ [alignLeft, expandContent] (h5 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u model)]) `styleBasic` [paddingV (0.25*u model)]
      | otherwise = box_ [alignLeft, expandContent] (h6 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u model)]) `styleBasic` [paddingV (0.25*u model)]
      -- | otherwise = error "Invalid heading level"

    convert (Node _ PARAGRAPH children) = hstack (map convert children)
    convert (Node _ STRONG [Node _ (TEXT t) _]) = bold model $ paragraph model t
    convert (Node _ EMPH [Node _ (TEXT t) _]) = paragraph model t
    convert (Node _ (TEXT t) _) = paragraph model t
    convert (Node _ (CODE t) _) = symbolSpan model t `styleBasic` [bgColor hoverColor, paddingV (0.25 * u model), paddingH (0.5 * u model), radius 8]
    convert (Node _ (LIST attr) children) = vstack_ [childSpacing_ (0.5 * u model)] (zipWith (\f i -> hstack [span model (getBullet i), convert f]) children [startNumber..]) `styleBasic` [paddingL (2 * u model)]
      where
        getBullet i
          | lt == BULLET_LIST = "• "
          | lt == ORDERED_LIST = showt i <> ". "
          | otherwise = ""
        lt = listType attr
        startNumber = listStart attr
    convert (Node _ ITEM children) = vstack (map convert children)
    convert (Node _ (LINK url _) [Node _ (TEXT t) _]) = externalLink t url
    convert (Node _ (IMAGE url _) _) = image url
    convert (Node _ BLOCK_QUOTE children) = vstack_ [childSpacing] (map convert children) `styleBasic` [paddingH (u model), paddingV (0.5*u model), bgColor hoverColor, border 1 dividerColor, radius 8]
    convert (Node _ (CODE_BLOCK _language content) _) = paragraph model content `styleBasic` [bgColor hoverColor, radius 8, padding (u model)]

    -- TODO
    convert (Node _ (CUSTOM_BLOCK _ _) _) = label ""
    convert (Node _ (CUSTOM_INLINE _ _) _) = label ""
    convert (Node _ (HTML_INLINE _) _) = label ""
    convert (Node _ (HTML_BLOCK _b) _) = label ""
    convert (Node _ SOFTBREAK _) = label ""
    convert (Node _ LINEBREAK _) = label ""
    convert (Node _ THEMATIC_BREAK _) = label ""
    
    -- Catch (hopefully) invalid configurations
    convert (Node _ (HEADING _) _) = label ""
    convert (Node _ STRONG _) = label ""
    convert (Node _ EMPH _) = label ""
    convert (Node _ (LINK _ _) _) = label ""
    
    -- convert f = error ("Missed: " ++ show f)