{-# LANGUAGE OverloadedStrings #-}

module Frontend.Components.MessageContainer
  ( messageContainer
  ) where

import Monomer
import Frontend.Types
import Data.Text (Text)

-- Create a container that registers message keys in the widget tree
messageContainer :: Text -> WidgetNode AppModel AppEvent -> WidgetNode AppModel AppEvent
messageContainer keyName content = 
  box_ [nodeKey (WidgetKey keyName), alignCenter, alignMiddle] content
    `styleBasic` [padding 0, bgColor transparent]
