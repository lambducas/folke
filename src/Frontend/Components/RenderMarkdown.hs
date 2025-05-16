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

renderMarkdown :: WidgetEnv AppModel AppEvent -> AppModel -> Text -> WidgetNode AppModel AppEvent
renderMarkdown wenv model markdown = widgetTree
  where
    widgetTree = convert nodes
    nodes = commonmarkToNode [] markdown

    selTheme = getActualTheme $ model ^. preferences . selectedTheme
    dividerColor = selTheme ^. L.userColorMap . at "dividerColor" . non def
    hoverColor = selTheme ^. L.userColorMap . at "hoverColor" . non def

    convert :: Node -> WidgetNode AppModel AppEvent
    convert (Node _ DOCUMENT children) = vstack_ [childSpacing] (map convert children)

    -- Headings
    convert (Node _ (HEADING n) [Node _ (TEXT t) _])
      | n == 1 = box_ [alignLeft, expandContent] (h1 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u model), textSize (3 * u model)]) `styleBasic` [paddingV (0.25*u model)]
      | n == 2 = box_ [alignLeft, expandContent] (h2 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u model)]) `styleBasic` [paddingT (1.5*u model), paddingB (0.25*u model)]
      | n == 3 = box_ [alignLeft, expandContent] (h3 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u model)]) `styleBasic` [paddingT (1.5*u model), paddingB (0.25*u model)]
      | n == 4 = box_ [alignLeft, expandContent] (h4 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u model)]) `styleBasic` [paddingT (1.5*u model), paddingB (0.25*u model)]
      | n == 5 = box_ [alignLeft, expandContent] (h5 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u model)]) `styleBasic` [paddingT (1.5*u model), paddingB (0.25*u model)]
      | otherwise = box_ [alignLeft, expandContent] (h6 model t `styleBasic` [borderB 1 dividerColor, paddingB (0.5 * u model)]) `styleBasic` [paddingT (1.5*u model), paddingB (0.25*u model)]
      -- | otherwise = error "Invalid heading level"

    -- Text
    convert (Node _ PARAGRAPH children) = hstack (map convert children)
    convert (Node _ STRONG [Node _ (TEXT t) _]) = bold model $ paragraph model t
    convert (Node _ EMPH [Node _ (TEXT t) _]) = paragraph model t
    convert (Node _ (TEXT t) _) = paragraph model t

    -- Inline code block
    convert (Node _ (CODE t) _) = symbolSpan model t `styleBasic` [bgColor hoverColor, paddingV (0.1 * u model), paddingH (0.5 * u model), radius 8]

    -- Blocks
    convert (Node _ BLOCK_QUOTE children) = vstack_ [childSpacing] (map convert children) `styleBasic` [paddingH (u model), paddingV (0.5*u model), bgColor hoverColor, border 1 dividerColor, radius 8]
    convert (Node _ (CODE_BLOCK _language content) _) = paragraph model content `styleBasic` [bgColor hoverColor, radius 8, padding (u model), logicTextFont model]
    
    -- Lists
    convert (Node _ (LIST attr) children) = vstack_ [childSpacing_ (0.5 * u model)] (zipWith (\f i -> hstack [span model (getBullet i), convert f]) children [startNumber..]) `styleBasic` [paddingL (2 * u model)]
      where
        getBullet i
          | lt == BULLET_LIST = "• "
          | lt == ORDERED_LIST = showt i <> ". "
          | otherwise = ""
        lt = listType attr
        startNumber = listStart attr
    convert (Node _ ITEM children) = vstack (map convert children)

    -- Links
    convert (Node _ (LINK url _) [Node _ (TEXT t) _])
      | url == "runinternalevent://CreateEmptyProof" = internalLink wenv model t CreateEmptyProof
      | otherwise = externalLink t url
    convert (Node _ (IMAGE url _) _) = image url

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