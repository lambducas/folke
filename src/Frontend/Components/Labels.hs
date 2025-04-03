module Frontend.Components.Labels (
  u,
  h1, h1_,
  h2, h2_,
  h3, h3_,
  h4, h4_,
  h5, h5_,
  h6, h6_,
  span, span_,
  symbolSpan, symbolSpan_,
  paragraph, paragraph_,
  iconLabel, iconLabel_,
  iconButton,
  trashButton,
  bold,
  normalStyle, symbolStyle
) where

import Prelude hiding (span)

import Frontend.Types
import Frontend.Themes
import Monomer
import qualified Monomer.Lens as L
import Control.Lens
import Data.Text (Text)
import Data.String (fromString)
import Data.Default (def)

-- Base unit for fonts. Use instead of pixels (1u = 16px)
u :: Double
u = 16

normalTextFont :: CmbTextFont t => AppModel -> t
normalTextFont model = textFont $ fromString $ model ^. preferences . normalFont

boldTextFont :: CmbTextFont t => AppModel -> t
boldTextFont model = textFont $ fromString $ last $ model ^. preferences . selectNormalFont

logicTextFont :: CmbTextFont t => AppModel -> t
logicTextFont model = textFont $ fromString $ model ^. preferences . logicFont

-- Using HTML tag name convention
h1, h2, h3, h4, h5, h6, span, paragraph, iconLabel, symbolSpan :: AppModel -> Text -> WidgetNode s e
h1_, h2_, h3_, h4_, h5_, h6_, span_, paragraph_, iconLabel_, symbolSpan_ :: AppModel -> Text -> [LabelCfg s e] -> WidgetNode s e

-- Main heading
h1 model t = label t `styleBasic` [ textSize (1.75 * u), boldTextFont model ]
h1_ model t cfg = label_ t cfg `styleBasic` [ textSize (1.5 * u), boldTextFont model ]

-- Secondary heading
h2 model t = label t `styleBasic` [ textSize (1.25 * u), boldTextFont model ]
h2_ model t cfg = label_ t cfg `styleBasic` [ textSize (1.25 * u), boldTextFont model ]

-- Smaller headings
h3 model t = label t `styleBasic` [ textSize (1.125 * u), boldTextFont model ]
h3_ model t cfg = label_ t cfg `styleBasic` [ textSize (1.125 * u), boldTextFont model ]

h4 model t = label t `styleBasic` [ textSize (1 * u), boldTextFont model ]
h4_ model t cfg = label_ t cfg `styleBasic` [ textSize (1 * u), boldTextFont model ]

h5 model t = label t `styleBasic` [ textSize (0.95 * u), boldTextFont model ]
h5_ model t cfg = label_ t cfg `styleBasic` [ textSize (0.95 * u), boldTextFont model ]

h6 model t = label t `styleBasic` [ textSize (0.9 * u), boldTextFont model ]
h6_ model t cfg = label_ t cfg `styleBasic` [ textSize (0.9 * u), boldTextFont model ]

-- Plain text
span model t = label t `styleBasic` [ textSize u, normalTextFont model ]
span_ model t cfg = label_ t cfg `styleBasic` [ textSize u, normalTextFont model ]

-- Monospaced text (used for symbols in logic)
symbolSpan model t = label t `styleBasic` [ textSize u, logicTextFont model ]
symbolSpan_ model t cfg = label_ t cfg `styleBasic` [ textSize u, logicTextFont model ]
-- symbolSpan t = label t `styleBasic` [ textSize u, textFont "Symbol_Regular" ]
-- symbolSpan_ t cfg = label_ t cfg `styleBasic` [ textSize u, textFont "Symbol_Regular" ]

-- Plain text when used as paragraph but can wrap to multiple lines (same as span right now but usually has margin at bottom and top)
paragraph model t = label_ t [multiline] `styleBasic` [ textSize u, normalTextFont model ]
paragraph_ model t cfg = label_ t cfg `styleBasic` [ textSize u, normalTextFont model ]

-- For rendering icons
iconLabel _model iconIdent = label iconIdent `styleBasic` [textFont "Remix", textBottom, textSize u]
iconLabel_ _model iconIdent cfg = label_ iconIdent cfg `styleBasic` [textFont "Remix", textBottom, textSize u]

-- For rendering icons inside buttons
iconButton :: AppModel -> Text -> AppEvent -> WidgetNode AppModel AppEvent
iconButton _model iconIdent action = button iconIdent action
  `styleBasic` [textFont "Remix", textMiddle, bgColor transparent, border 1 transparent, textSize u]

-- Button with trashcan icon
trashButton :: AppModel -> AppEvent -> WidgetNode AppModel AppEvent
trashButton model action = iconButton model remixDeleteBinFill action
  `styleBasic` [textColor orangeRed, textSize u]
  `styleFocus` [border 1 (rgba 255 20 0 0.75)]
  `styleHover` [bgColor hoverColor]
  `styleDisabled` [textColor (rgba 255 220 200 0.3)]
  where
    hoverColor = selTheme ^. L.userColorMap . at "hoverColor" . non def
    selTheme = getActualTheme $ model ^. preferences . selectedTheme

-- Make widget bold
bold :: CmbStyleBasic t => AppModel -> t -> t
bold model widget = widget `styleBasic` [ boldTextFont model ]

-- Make general widgets adhere to font and text size change
normalStyle :: AppModel -> WidgetNode s e -> WidgetNode s e
normalStyle model widget = widget `styleBasic` [textSize u, normalTextFont model]

symbolStyle :: AppModel -> WidgetNode s e -> WidgetNode s e
symbolStyle model widget = widget `styleBasic` [textSize u, logicTextFont model]