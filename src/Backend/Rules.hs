module Backend.Rules (
    ruleReiteration,
    ruleAndIntro,
    ruleAndElimLeft,
    ruleAndElimRight
) where

import qualified Data.List as List
import Backend.Types



ruleReiteration:: [Formula] -> Result Formula
ruleReiteration [form] = Ok(form)
ruleReiteration forms  = Error TypeError ("Reiteration takes 1 argument not" ++ show (List.length forms) ++".")

ruleAndIntro:: [Formula] -> Result Formula
ruleAndIntro [a, b] = Ok (And a b)
ruleAndIntro forms  = Error TypeError ("And Introduction takes 2 arguments not" ++ show (List.length forms) ++".")

ruleAndElimLeft::  [Formula] -> Result Formula
ruleAndElimLeft [form] = case form of
    And l r -> Ok l
    _ -> Error TypeError ("Can not apply And elimination on " ++ show form ++ ".")
ruleAndElimLeft forms  = Error TypeError ("And elimination takes 2 arguments not" ++ show (List.length forms) ++".")

ruleAndElimRight:: [Formula] -> Result Formula
ruleAndElimRight [form] = case form of
    And l r -> Ok r
    _ -> Error TypeError ("Can not apply And elimination on " ++ show form ++ ".")
ruleAndElimRight forms  = Error TypeError ("And elimination takes 2 arguments not" ++ show (List.length forms) ++".")