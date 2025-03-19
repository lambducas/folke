module Backend.Rules (
    ruleReiteration,
    ruleAndIntro,
    ruleAndElimLeft,
    ruleAndElimRight,
    ruleOrIntroLeft,
    ruleOrIntroRight,
    ruleOrEilm,
    ruleThenIntro,
    ruleThenEilm,
    ruleNotIntro,
    ruleNotEilm,
    ruleBottomElim,
    ruleNotNotIntro,
    ruleNotNotElim
) where

import qualified Data.List as List
import Backend.Types



ruleReiteration:: [Formula] -> Formula -> Result Formula
ruleReiteration [form] res = Ok(form)
ruleReiteration forms res  = Error TypeError ("Reiteration takes 1 argument not" ++ show (List.length forms) ++".")

ruleAndIntro:: [Formula] -> Formula -> Result Formula
ruleAndIntro [a, b] res = Ok (And a b)
ruleAndIntro forms res  = Error TypeError ("And Iintroduction takes 2 arguments not" ++ show (List.length forms) ++".")

ruleAndElimLeft::  [Formula] -> Formula -> Result Formula
ruleAndElimLeft [form] res = case form of
    And l r -> Ok l
    _ -> Error TypeError ("Can not apply And elimination on " ++ show form ++ ".")
ruleAndElimLeft forms res  = Error TypeError ("And Elimination takes 2 arguments not" ++ show (List.length forms) ++".")

ruleAndElimRight:: [Formula] -> Formula -> Result Formula
ruleAndElimRight [form] res = case form of
    And l r -> Ok r
    _ -> Error TypeError ("Can not apply And elimination on " ++ show form ++ ".")
ruleAndElimRight forms res = Error TypeError ("And Elimination takes 2 arguments not" ++ show (List.length forms) ++".")

ruleOrIntroLeft:: [Formula] -> Formula -> Result Formula
ruleOrIntroLeft [a]  r@(Or b _) = if a == b then Ok r else Error TypeError (show a ++ "did not match " ++ show b)
ruleOrIntroLeft [a]  r = Error TypeError (show r ++" is not an an or.")
ruleOrIntroLeft forms _  = Error TypeError ("Or introduction takes 1 argument not" ++ show (List.length forms) ++".")

ruleOrIntroRight:: [Formula] -> Formula -> Result Formula
ruleOrIntroRight [a]  r@(Or _ b) = if a == b then Ok r else Error TypeError (show a ++ "did not match " ++ show b)
ruleOrIntroRight [a]  r = Error TypeError (show r ++" is not an an or.")
ruleOrIntroRight forms _  = Error TypeError ("Or introduction takes 1 argument not" ++ show (List.length forms) ++".")

ruleOrEilm:: [Formula] -> Formula -> Result Formula
ruleOrEilm _ _ = Error TypeError "Or eliminaton is not implemented"

ruleThenIntro:: [Formula] -> Formula -> Result Formula
ruleThenIntro _ _ = Error TypeError "Or introduction is not implemented"
ruleThenEilm:: [Formula] -> Formula -> Result Formula
ruleThenEilm _ _ = Error TypeError "Or eliminaton is not implemented"

ruleNotIntro:: [Formula] -> Formula -> Result Formula
ruleNotIntro _ _ = Error TypeError "Not introduction is not implemented"
ruleNotEilm:: [Formula] -> Formula -> Result Formula
ruleNotEilm _ _ = Error TypeError "Not eliminaton is not implemented"

ruleBottomElim:: [Formula] -> Formula -> Result Formula
ruleBottomElim [Bot] r = Ok r
ruleBottomElim [_a] _ = Error TypeError "Argument needs to be bottom"
ruleBottomElim forms _  = Error TypeError ("Bottom eliminaton takes 1 argument not" ++ show (List.length forms) ++".")

ruleNotNotIntro:: [Formula] -> Formula -> Result Formula
ruleNotNotIntro [a] _ = Ok (Not (Not a))
ruleNotNotIntro forms _  = Error TypeError ("Not not introduction takes 1 argument not" ++ show (List.length forms) ++".")
ruleNotNotElim:: [Formula] -> Formula -> Result Formula
ruleNotNotElim [Not (Not a)] _ = Ok a
ruleNotNotElim [_a] _ = Error TypeError "Argument needs to be not not"
ruleNotNotElim forms _  = Error TypeError ("Not not eliminaton takes 1 argument not" ++ show (List.length forms) ++".")