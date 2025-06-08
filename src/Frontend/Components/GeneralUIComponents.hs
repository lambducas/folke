module Frontend.Components.GeneralUIComponents where

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
u :: AppModel -> Double
u model = model ^. preferences . fontSize
-- u = 16

-- | Get `StyleState` for normal text
normalTextFont :: CmbTextFont t => AppModel -> t
normalTextFont model = textFont $ fromString $ model ^. preferences . normalFont

-- | Get `StyleState` for bold text
boldTextFont :: CmbTextFont t => AppModel -> t
boldTextFont model = textFont $ fromString $ last $ model ^. preferences . selectNormalFont

-- | Get `StyleState` for monospace/symbol text
logicTextFont :: CmbTextFont t => AppModel -> t
logicTextFont model = textFont $ fromString $ model ^. preferences . logicFont

h1, h2, h3, h4, h5, h6, span, paragraph, iconLabel, symbolSpan :: AppModel -> Text -> WidgetNode s e
h1_, h2_, h3_, h4_, h5_, h6_, span_, paragraph_, iconLabel_, symbolSpan_ :: AppModel -> Text -> [LabelCfg s e] -> WidgetNode s e

-- | Main heading
h1 model t = label t `styleBasic` [ textSize (1.75 * u model), boldTextFont model ]
h1_ model t cfg = label_ t cfg `styleBasic` [ textSize (1.75 * u model), boldTextFont model ]

-- | Secondary heading
h2 model t = label t `styleBasic` [ textSize (1.25 * u model), boldTextFont model ]
h2_ model t cfg = label_ t cfg `styleBasic` [ textSize (1.25 * u model), boldTextFont model ]

-- | Level 3 heading
h3 model t = label t `styleBasic` [ textSize (1.125 * u model), boldTextFont model ]
h3_ model t cfg = label_ t cfg `styleBasic` [ textSize (1.125 * u model), boldTextFont model ]

-- | Level 4 heading
h4 model t = label t `styleBasic` [ textSize (1 * u model), boldTextFont model ]
h4_ model t cfg = label_ t cfg `styleBasic` [ textSize (1 * u model), boldTextFont model ]

-- | Level 5 heading
h5 model t = label t `styleBasic` [ textSize (0.95 * u model), boldTextFont model ]
h5_ model t cfg = label_ t cfg `styleBasic` [ textSize (0.95 * u model), boldTextFont model ]

-- | Level 6 heading
h6 model t = label t `styleBasic` [ textSize (0.9 * u model), boldTextFont model ]
h6_ model t cfg = label_ t cfg `styleBasic` [ textSize (0.9 * u model), boldTextFont model ]

-- | Plain text without word-wrap
span model t = label t `styleBasic` [ textSize (u model), normalTextFont model ]
span_ model t cfg = label_ t cfg `styleBasic` [ textSize (u model), normalTextFont model ]

-- | Monospaced text (used for symbols in logic) without word-wrap
symbolSpan model t = label t `styleBasic` [ textSize (u model), logicTextFont model ]
symbolSpan_ model t cfg = label_ t cfg `styleBasic` [ textSize (u model), logicTextFont model ]
-- symbolSpan t = label t `styleBasic` [ textSize u, textFont "Symbol_Regular" ]
-- symbolSpan_ t cfg = label_ t cfg `styleBasic` [ textSize u, textFont "Symbol_Regular" ]

{-|
Plain text when used as paragraph but can wrap to multiple lines
(Usually has margin at bottom and top but this is missing right now)
-}
paragraph model t = label_ t [multiline] `styleBasic` [ textSize (u model), normalTextFont model ]
paragraph_ model t cfg = label_ t cfg `styleBasic` [ textSize (u model), normalTextFont model ]

-- | For rendering icons
iconLabel model iconIdent = label iconIdent `styleBasic` [textFont "Remix", textBottom, textSize (u model)]
iconLabel_ model iconIdent cfg = label_ iconIdent cfg `styleBasic` [textFont "Remix", textBottom, textSize (u model)]

-- | For rendering icons inside buttons
iconButton :: AppModel -> Text -> AppEvent -> WidgetNode AppModel AppEvent
iconButton model iconIdent action = Frontend.Components.GeneralUIComponents.button model iconIdent action
  `styleBasic` [textFont "Remix", textMiddle, textSize (u model), bgColor transparent, border 1 transparent]
  `styleHover` [bgColor hoverColor]
  where
    hoverColor = selTheme ^. L.userColorMap . at "hoverColor" . non def
    selTheme = getActualTheme $ model ^. preferences . selectedTheme

-- | For rendering icons inside toggle buttons
iconToggleButton :: AppModel -> Text -> ALens' AppModel Bool -> WidgetNode AppModel AppEvent
iconToggleButton model iconIdent l = iconToggleButton_ model iconIdent l []

iconToggleButton_ :: AppModel -> Text -> ALens' AppModel Bool -> [ToggleButtonCfg AppModel AppEvent] -> WidgetNode AppModel AppEvent
iconToggleButton_ model iconIdent l cfg = toggleButton_ iconIdent l (toggleButtonOffStyle offStyle : cfg)
  `styleBasic` [textFont "Remix", textMiddle, textSize (u model), textColor accentColor, bgColor selectedColor, border 0 transparent]
  where
    offStyle = def
      `styleBasic` [bgColor transparent, border 0 transparent]
      `styleHover` [bgColor hoverColor]
    hoverColor = selTheme ^. L.userColorMap . at "hoverColor" . non def
    selectedColor = selTheme ^. L.userColorMap . at "selectedFileBg" . non def
    accentColor = selTheme ^. L.userColorMap . at "accent" . non def
    selTheme = getActualTheme $ model ^. preferences . selectedTheme

-- | Button with trashcan icon
trashButton :: AppModel -> AppEvent -> WidgetNode AppModel AppEvent
trashButton model action = Monomer.button remixDeleteBinFill action
  `styleBasic` [textFont "Remix", textMiddle, textSize (u model), textColor orangeRed, bgColor transparent, border 0 transparent]
  `styleFocus` [border 1 (rgba 255 20 0 0.75)]
  `styleHover` [bgColor hoverColor]
  `styleDisabled` [textColor (rgba 255 220 200 0.3)]
  where
    hoverColor = selTheme ^. L.userColorMap . at "hoverColor" . non def
    selTheme = getActualTheme $ model ^. preferences . selectedTheme

-- | Make widget text bold
bold :: CmbStyleBasic t => AppModel -> t -> t
bold model widget = widget `styleBasic` [ boldTextFont model ]

-- | Make general widgets adhere to normal font and text size change
normalStyle :: AppModel -> WidgetNode s e -> WidgetNode s e
normalStyle model widget = widget `styleBasic` [textSize (u model), normalTextFont model]

-- | Make general widgets adhere to symbol font and text size change
symbolStyle :: AppModel -> WidgetNode s e -> WidgetNode s e
symbolStyle model widget = widget `styleBasic` [textSize (u model), logicTextFont model]

-- | Default button with user-selected fontsize
button :: AppModel -> Text -> AppEvent -> WidgetNode AppModel AppEvent
button model a b = Monomer.button a b `styleBasic` [textSize (u model)]

-- | Appears faster than the default tooltip and is scaled by user-selected fontsize
fastTooltip :: AppModel -> Text -> WidgetNode s e -> WidgetNode s e
fastTooltip model tip widget = Monomer.tooltip_ tip [tooltipDelay 400] widget `styleBasic` [textSize (u model)]

fastScroll, fastVScroll, fastHScroll :: WidgetNode s e -> WidgetNode s e

-- | `scroll` but with more reasonable scroll rate
fastScroll = scroll_ [wheelRate 50]

-- | `vscroll` but with more reasonable scroll rate
fastVScroll = fastVScroll_ []

-- | `vscroll_` but with more reasonable scroll rate
fastVScroll_ :: [ScrollCfg s e] -> WidgetNode s e -> WidgetNode s e
fastVScroll_ cfg = vscroll_ (wheelRate 50 : cfg)

-- | `hscroll` but with more reasonable scroll rate
fastHScroll = fastHScroll_ []

-- | `hscroll_` but with more reasonable scroll rate
fastHScroll_ :: [ScrollCfg s e] -> WidgetNode s e -> WidgetNode s e
fastHScroll_ cfg = hscroll_ (wheelRate 50 : cfg)

boxShadow :: WidgetNode s e -> WidgetNode s e
boxShadow = boxShadow_ [radius 8]

internalLink :: WidgetEnv AppModel AppEvent -> AppModel -> Text -> AppEvent -> WidgetNode AppModel AppEvent
internalLink wenv model t event = box_ [onClick event] textWidget
  where
    textWidget = span model t & L.info . L.style .~ style
    style = collectTheme wenv L.externalLinkStyle

{-|
Will handle keystrokes from start to end of list and stop when a
key-combination matches currently presssed keys (if it is enabled).
Children will not receive the key-event
-}
firstKeystroke :: [(Text, AppEvent, Bool)] -> WidgetNode s AppEvent -> WidgetNode s AppEvent
firstKeystroke ((key, event, enabled):xs) widget = keystroke_ [(key, if enabled then event else NoEvent)] [ignoreChildrenEvts | enabled] (firstKeystroke xs widget)
firstKeystroke [] widget = widget

-- | Like keystroke but shortcuts can be enabled/disabled
someKeystrokes :: [(Text, AppEvent, Bool)] -> WidgetNode s AppEvent -> WidgetNode s AppEvent
someKeystrokes ((key, event, enabled):xs) widget = keystroke_ [(key, if enabled then event else NoEvent)] [] (someKeystrokes xs widget)
someKeystrokes [] widget = widget