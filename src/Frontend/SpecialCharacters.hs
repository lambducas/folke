{-# LANGUAGE OverloadedStrings #-}

module Frontend.SpecialCharacters (
  replaceFromLookup,
  replaceFromInverseLookup,
  replaceSpecialSymbols,
  replaceSpecialSymbolsInverse
) where

import Frontend.Types ( SymbolDict )
import Data.Text (Text, replace)

symbolLookup :: SymbolDict
symbolLookup = [
  -- Negate
  ("!", "¬"),
  ("~", "¬"),
  ("-", "¬"),

  -- Implies
  (">", "→"),
  ("->", "→"),

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
  ("all", "∀"),
  ("some", "∃"),
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