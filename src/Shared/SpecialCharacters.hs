{-# LANGUAGE OverloadedStrings #-}

module Shared.SpecialCharacters (
  replaceFromLookup,
  replaceFromInverseLookup,
  replaceSpecialSymbols,
  replaceSpecialSymbolsInverse
) where

import Data.Text (Text, replace)

type SymbolDict = [(Text, Text)]

symbolLookup :: SymbolDict
symbolLookup = [
  -- Negate
  ("!", "¬"),
  ("~", "¬"),
  ("not", "¬"),

  -- Implies
  (">", "→"),
  ("->", "→"),
  ("¬>", "→"), -- Fix for when "-" becomes negation

  -- And
  ("&", "∧"),
  ("^", "∧"),
  ("*", "∧"),
  ("and", "∧"),
  ("con", "∧"),

  -- Or
  ("|", "∨"),
  --("v", "∨"),
  ("+", "∨"),
  ("f∨", "for"), -- Disable `or` replacement when writing forall
  ("or", "∨"),
  ("dis", "∨"),

  -- Bottom
  ("bot", "⊥"),
  ("#", "⊥"),
  ("XX", "⊥"),

  -- For all
  ("all", "∀"),
  ("forall", "∀"),
  -- ("A", "∀"),

  -- There exists
  ("exists", "∃"),
  ("some", "∃"),
  -- ("E", "∃"),

  -- Sequent
  ("=/>", "⊬"),
  ("=>", "⊢"),

  -- Subscript
  ("_0", "₀"),
  ("_1", "₁"),
  ("_2", "₂"),
  ("_3", "₃"),
  ("_4", "₄"),
  ("_5", "₅"),
  ("_6", "₆"),
  ("_7", "₇"),
  ("_8", "₈"),
  ("_9", "₉")
  ]

symbolLookupInverse :: SymbolDict
symbolLookupInverse = [
  ("!", "¬"),
  ("->", "→"),
  ("&", "∧"),
  ("|", "∨"),
  ("bot", "⊥"),
  -- ("all", "∀"),
  -- ("some", "∃"),
  ("=/>", "⊬"),
  ("=>", "⊢"),
  ("_0", "₀"),
  ("_1", "₁"),
  ("_2", "₂"),
  ("_3", "₃"),
  ("_4", "₄"),
  ("_5", "₅"),
  ("_6", "₆"),
  ("_7", "₇"),
  ("_8", "₈"),
  ("_9", "₉")
  ]

replaceFromLookup :: Text -> SymbolDict -> Text
replaceFromLookup s [] = s
replaceFromLookup s ((key, value):ls) = replace key value $ replaceFromLookup s ls

replaceFromInverseLookup :: Text -> SymbolDict -> Text
replaceFromInverseLookup s [] = s
replaceFromInverseLookup s ((key, value):ls) = replace value key $ replaceFromInverseLookup s ls

replaceSpecialSymbols :: Text -> Text
replaceSpecialSymbols s = replaceFromLookup s symbolLookup

replaceSpecialSymbolsInverse :: Text -> Text
replaceSpecialSymbolsInverse s = replaceFromInverseLookup s symbolLookupInverse