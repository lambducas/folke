module Frontend.Components.Labels (
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
import Monomer
import Control.Lens
import Data.Text (Text)
import Data.String (fromString)

-- Base unit for fonts. Use instead of pixels (1u = 16px)
-- u :: Double
-- u = model ^. fontSize = 16

-- Using HTML tag name convention
h1, h2, h3, h4, h5, h6, span, paragraph, iconLabel, symbolSpan :: AppModel -> Text -> WidgetNode s e
h1_, h2_, h3_, h4_, h5_, h6_, span_, paragraph_, iconLabel_, symbolSpan_ :: AppModel -> Text -> [LabelCfg s e] -> WidgetNode s e

-- Main heading
h1 model t = label t `styleBasic` [ textSize (1.75 * (model ^. fontSize)), textFont $ fromString $ last $ model ^. selectNormalFont ]
h1_ model t cfg = label_ t cfg `styleBasic` [ textSize (1.5 * (model ^. fontSize)), textFont $ fromString $ last $ model ^. selectNormalFont ]

-- Secondary heading
h2 model t = label t `styleBasic` [ textSize (1.25 * (model ^. fontSize)), textFont $ fromString $ last $ model ^. selectNormalFont ]
h2_ model t cfg = label_ t cfg `styleBasic` [ textSize (1.25 * (model ^. fontSize)), textFont $ fromString $ last $ model ^. selectNormalFont ]

-- Smaller headings
h3 model t = label t `styleBasic` [ textSize (1.125 * (model ^. fontSize)), textFont $ fromString $ last $ model ^. selectNormalFont ]
h3_ model t cfg = label_ t cfg `styleBasic` [ textSize (1.125 * (model ^. fontSize)), textFont $ fromString $ last $ model ^. selectNormalFont ]

h4 model t = label t `styleBasic` [ textSize (1 * (model ^. fontSize)), textFont $ fromString $ last $ model ^. selectNormalFont ]
h4_ model t cfg = label_ t cfg `styleBasic` [ textSize (1 * (model ^. fontSize)), textFont $ fromString $ last $ model ^. selectNormalFont ]

h5 model t = label t `styleBasic` [ textSize (0.95 * (model ^. fontSize)), textFont $ fromString $ last $ model ^. selectNormalFont ]
h5_ model t cfg = label_ t cfg `styleBasic` [ textSize (0.95 * (model ^. fontSize)), textFont $ fromString $ last $ model ^. selectNormalFont ]

h6 model t = label t `styleBasic` [ textSize (0.9 * (model ^. fontSize)), textFont $ fromString $ last $ model ^. selectNormalFont ]
h6_ model t cfg = label_ t cfg `styleBasic` [ textSize (0.9 * (model ^. fontSize)), textFont $ fromString $ last $ model ^. selectNormalFont ]

-- Plain text
span model t = label t `styleBasic` [ textSize (model ^. fontSize), textFont $ fromString $ model ^. normalFont ]
span_ model t cfg = label_ t cfg `styleBasic` [ textSize (model ^. fontSize), textFont $ fromString $ model ^. normalFont ]

-- Monospaced text (used for symbols in logic)
symbolSpan model t = label t `styleBasic` [ textSize (model ^. fontSize), textFont $ fromString $ model ^. logicFont ]
symbolSpan_ model t cfg = label_ t cfg `styleBasic` [ textSize (model ^. fontSize), textFont $ fromString $ model ^. logicFont ]
-- symbolSpan t = label t `styleBasic` [ textSize u, textFont "Symbol_Regular" ]
-- symbolSpan_ t cfg = label_ t cfg `styleBasic` [ textSize u, textFont "Symbol_Regular" ]

-- Plain text when used as paragraph but can wrap to multiple lines (same as span right now but usually has margin at bottom and top)
paragraph model t = label_ t [multiline] `styleBasic` [ textSize (model ^. fontSize), textFont $ fromString $ model ^. normalFont ]
paragraph_ model t cfg = label_ t cfg `styleBasic` [ textSize (model ^. fontSize), textFont $ fromString $ model ^. normalFont ]

-- For rendering icons
iconLabel model iconIdent = label iconIdent `styleBasic` [textFont "Remix", textBottom, textSize $ model ^. fontSize]
iconLabel_ model iconIdent cfg = label_ iconIdent cfg `styleBasic` [textFont "Remix", textBottom, textSize $ model ^. fontSize]

-- For rendering icons inside buttons
iconButton :: AppModel -> Text -> AppEvent -> WidgetNode AppModel AppEvent
iconButton model iconIdent action = button iconIdent action
  `styleBasic` [textFont "Remix", textMiddle, bgColor transparent, border 0 transparent, textSize $ model ^. fontSize]

-- Button with trashcan icon
trashButton :: AppModel -> AppEvent -> WidgetNode AppModel AppEvent
trashButton model action = iconButton model remixDeleteBinFill action
  `styleBasic` [textColor orangeRed, textSize $ model ^. fontSize]

-- Make widget bold
bold :: CmbStyleBasic t => AppModel -> t -> t
bold model widget = widget `styleBasic` [ textFont $ fromString $ last $ model ^. selectNormalFont ]

-- Make general widgets adhere to font and text size change
normalStyle :: AppModel -> WidgetNode s e -> WidgetNode s e
normalStyle model widget = widget `styleBasic` [textFont $ fromString $ model ^. normalFont, textSize $ model ^. fontSize]

symbolStyle :: AppModel -> WidgetNode s e -> WidgetNode s e
symbolStyle model widget = widget `styleBasic` [textFont $ fromString $ model ^. logicFont, textSize $ model ^. fontSize]