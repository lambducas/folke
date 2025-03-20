module Backend.Rules (
    ruleReiteration,
    ruleAndIntro,
    ruleAndElimLeft,
    ruleAndElimRight,
    ruleOrIntroLeft,
    ruleOrIntroRight,
    ruleOrEilm,
    ruleIfIntro,
    ruleIfEilm,
    ruleNotIntro,
    ruleNotEilm,
    ruleBottomElim,
    ruleNotNotIntro,
    ruleNotNotElim,
    ruleMT,
    rulePBC,
    ruleLEM
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

ruleIfIntro:: [Formula] -> Formula -> Result Formula
ruleIfIntro _ _ = Error TypeError "Or introduction is not implemented"
ruleIfEilm:: [Formula] -> Formula -> Result Formula
ruleIfEilm [a, If b c] r = if a == b then if c == r then Ok r
        else Error TypeError ("Expected result " ++ show r ++ " did not match result of rule " ++ show b ++ ".")
    else Error TypeError (show a ++ " did not match " ++ show b ++ ".")
ruleIfEilm [_, _] _  = Error TypeError "If eliminaton takes a argument on the form A->B"
ruleIfEilm forms _  = Error TypeError ("If eliminaton takes 2 argument not" ++ show (List.length forms) ++".")

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

ruleMT:: [Formula] -> Formula -> Result Formula
ruleMT [If a b, Not c] r@(Not d) = if b == c && a==d then Ok r else Error TypeError "Arguments did not match"
ruleMT forms _  = Error TypeError ("MT takes 2 argument not" ++ show (List.length forms) ++".")

rulePBC:: [Formula] -> Formula -> Result Formula
rulePBC _ _ = Error TypeError "MT is not implemented"
ruleLEM:: [Formula] -> Formula -> Result Formula
ruleLEM [] r@(Or a (Not b)) = if a == b then Ok r else Error TypeError "Arguments did not match"
ruleLEM forms _  = Error TypeError ("LEM takes 0 argument not" ++ show (List.length forms) ++".")