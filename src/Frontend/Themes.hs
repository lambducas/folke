module Frontend.Themes (
  customLightTheme,
  customDarkTheme,
  getActualTheme
) where

import Frontend.Types
import Monomer
import Monomer.Core.Themes.BaseTheme
import qualified Monomer.Lens as L
import Control.Lens

-- customTheme :: Theme
-- customTheme = baseTheme lightThemeColors {
--   btnMainBgBasic = rgbHex "#EE9000",
--   btnMainBgHover = rgbHex "#FFB522",
--   btnMainBgFocus = rgbHex "#FFA500",
--   btnMainBgActive = rgbHex "#DD8000",
--   btnMainBgDisabled = rgbHex "#BB8800",
--   btnMainText = rgbHex "000000"
-- }

customLightTheme :: Theme
customLightTheme = baseTheme lightThemeColors {
  clearColor = rgb 255 255 255,

  btnBgBasic = rgb 235 235 235,
  btnBgHover = rgb 210 210 210,
  btnBgFocus = rgb 220 220 220,
  btnBgActive = rgb 190 190 190,
  btnBgDisabled = rgb 150 150 150,
  btnTextDisabled = rgb 70 70 70,
  btnText = rgbHex "000000",
  labelText = rgbHex "000000"

  -- btnMainBgBasic = rgbHex "#EE9000",
  -- btnMainBgHover = rgbHex "#FFB522",
  -- btnMainBgFocus = rgbHex "#FFA500",
  -- btnMainBgActive = rgbHex "#DD8000",
  -- btnMainBgDisabled = rgbHex "#BB8800",
  -- btnMainText = rgbHex "#FF0000"
}
  & L.userColorMap . at "accent" ?~ rgb 255 65 43
  & L.userColorMap . at "popupBackground" ?~ rgb 230 230 230
  & L.userColorMap . at "backgroundColor" ?~ rgb 255 255 255
  & L.userColorMap . at "hoverColor" ?~ rgba 0 0 0 0.05
  & L.userColorMap . at "selectedFileBg" ?~ rgba 0 0 0 0.1
  & L.userColorMap . at "dividerColor" ?~ rgba 0 0 0 0.1
  & L.userColorMap . at "proofBoxColor" ?~ rgba 0 0 0 0.3

customDarkTheme :: Theme
customDarkTheme = baseTheme darkThemeColors {
  clearColor = rgb 30 30 30,

  inputBgBasic = rgb 50 50 50,

  btnBgBasic = rgb 50 50 50,
  btnBgHover = rgb 70 70 70,
  btnBgFocus = rgb 60 60 60,
  btnBgActive = rgb 90 90 90,
  btnBgDisabled = rgb 40 40 40,
  btnTextDisabled = rgb 70 70 70,
  btnText = rgbHex "#FFFFFF",
  labelText = rgbHex "#FFFFFF"

  -- btnMainBgBasic = rgbHex "#EE9000",
  -- btnMainBgHover = rgbHex "#0000FF",
  -- btnMainBgFocus = rgbHex "#FFA500",
  -- btnMainBgActive = rgbHex "#DD8000",
  -- btnMainBgDisabled = rgbHex "#BB8800",
  -- btnMainText = rgbHex "#FF0000"
}
  & L.userColorMap . at "accent" ?~ rgb 224 76 58
  & L.userColorMap . at "popupBackground" ?~ rgb 50 50 50
  & L.userColorMap . at "backgroundColor" ?~ rgb 30 30 30
  & L.userColorMap . at "hoverColor" ?~ rgba 255 255 255 0.05
  & L.userColorMap . at "selectedFileBg" ?~ rgba 255 255 255 0.1
  & L.userColorMap . at "dividerColor" ?~ rgba 255 255 255 0.1
  & L.userColorMap . at "proofBoxColor" ?~ rgba 255 255 255 0.3

getActualTheme :: SelectableTheme -> Theme
getActualTheme Light = customLightTheme
getActualTheme Dark = customDarkTheme