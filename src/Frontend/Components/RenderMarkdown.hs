{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.Components.RenderMarkdown (
  renderMarkdown
) where

import Prelude hiding (span)

import Frontend.Types
import Frontend.Helper
import Frontend.Components.Labels
import Monomer
import Data.Text (Text, splitOn, unpack, pack, intercalate)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Lens

renderMarkdown :: AppModel -> Text -> WidgetNode s e
renderMarkdown model markdown = vstack_ [childSpacing] lineRenders
  where
    lineRenders = map (renderLine . trim . unpack) lines
    lines = splitOn "\n" markdown
    renderLine ('*':t) = span model ("• " <> pack (trim t)) `styleBasic` [paddingL (model ^. fontSize)]
    renderLine ('#':'#':'#':'#':'#':'#':t) = h6 model (pack (trim t))
    renderLine ('#':'#':'#':'#':'#':t) = h5 model (pack (trim t))
    renderLine ('#':'#':'#':'#':t) = h4 model (pack (trim t))
    renderLine ('#':'#':'#':t) = h3 model (pack (trim t))
    renderLine ('#':'#':t) = h2 model (pack (trim t))
    renderLine ('#':t) = h1 model (pack (trim t))
    renderLine t
      | isList = span model (liA <> ". " <> liB)-- `styleBasic` [paddingL (model ^. fontSize)]
      | otherwise = paragraph model (pack (trim t))
      where
        isList = 
          length splitDot > 1 &&
          '-' `notElem` unpack liA &&
          '(' `notElem` unpack liA &&
          ')' `notElem` unpack liA &&
          fromMaybe False ((readMaybe (unpack liA) :: Maybe Int) >>= (\f -> Just $ f > 0))
        liA = head splitDot
        liB = intercalate "." (tail splitDot)
        splitDot = splitOn "." tt
        tt = pack t