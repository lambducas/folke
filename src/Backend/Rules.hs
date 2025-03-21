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



ruleReiteration:: [Arg] -> Formula -> Result Formula
ruleReiteration [ArgForm form] _ = Ok form
ruleReiteration forms _ = Error TypeError ("Reiteration takes 1 argument not " ++ show (List.length forms) ++".")

ruleAndIntro:: [Arg] -> Formula -> Result Formula
ruleAndIntro [ArgForm a, ArgForm b] _ = Ok (And a b) 
ruleAndIntro forms _  = Error TypeError ("And Iintroduction takes 2 arguments not " ++ show (List.length forms) ++".")

ruleAndElimLeft::  [Arg] -> Formula -> Result Formula
ruleAndElimLeft [ArgForm form] _ = case form of
    And l _ -> Ok l
    _ -> Error TypeError ("Can not apply And elimination on " ++ show form ++ ".")
ruleAndElimLeft forms _  = Error TypeError ("And Elimination takes 2 arguments not " ++ show (List.length forms) ++".")

ruleAndElimRight:: [Arg] -> Formula -> Result Formula
ruleAndElimRight [ArgForm form] _ = case form of
    And _ r -> Ok r
    _ -> Error TypeError ("Can not apply And elimination on " ++ show form ++ ".")
ruleAndElimRight forms _ = Error TypeError ("And Elimination takes 2 arguments not " ++ show (List.length forms) ++".")

ruleOrIntroLeft:: [Arg] -> Formula -> Result Formula
ruleOrIntroLeft [ArgForm a]  r@(Or b _) = if a == b then Ok r else Error TypeError (show a ++ "did not match " ++ show b)
ruleOrIntroLeft [_]  r = Error TypeError (show r ++" is not an an or.")
ruleOrIntroLeft forms _  = Error TypeError ("Or introduction takes 1 argument not " ++ show (List.length forms) ++".")

ruleOrIntroRight:: [Arg] -> Formula -> Result Formula
ruleOrIntroRight [ArgForm a]  r@(Or _ b) = if a == b then Ok r else Error TypeError (show a ++ "did not match " ++ show b)
ruleOrIntroRight [_]  r = Error TypeError (show r ++" is not an an or.")
ruleOrIntroRight forms _  = Error TypeError ("Or introduction takes 1 argument not " ++ show (List.length forms) ++".")

ruleOrEilm:: [Arg] -> Formula -> Result Formula
ruleOrEilm [ArgForm (Or a b), ArgProof (Proof [p1] c1), ArgProof (Proof [p2] c2)] _ = if a == p1 && b == p2 && c1 == c2 then Ok c1 else Error TypeError "Error"
ruleOrEilm forms _  = Error TypeError ("Or eliminaton takes 3 argument not " ++ show (List.length forms) ++".")

ruleIfIntro:: [Arg] -> Formula -> Result Formula
ruleIfIntro [ArgProof (Proof [a] b)] _ = Ok (If a b)
ruleIfIntro forms _  = Error TypeError ("If introduction takes 1 argument not " ++ show (List.length forms) ++".")

ruleIfEilm:: [Arg] -> Formula -> Result Formula
ruleIfEilm [ArgForm a, ArgForm (If b c)] r = if a == b then if c == r then Ok r
        else Error TypeError ("Expected result " ++ show r ++ " did not match result of rule " ++ show b ++ ".")
    else Error TypeError (show a ++ " did not match " ++ show b ++ ".")
ruleIfEilm [_, _] _  = Error TypeError "If eliminaton takes a argument on the form A->B"
ruleIfEilm forms _  = Error TypeError ("If eliminaton takes 2 argument not " ++ show (List.length forms) ++".")

ruleNotIntro:: [Arg] -> Formula -> Result Formula
ruleNotIntro [ArgProof (Proof [a] Bot)] _ = Ok (Not a)
ruleNotIntro forms _  = Error TypeError ("Not introduction takes 1 argument not " ++ show (List.length forms) ++".")
ruleNotEilm:: [Arg] -> Formula -> Result Formula
ruleNotEilm [ArgForm a, ArgForm (Not b)] _ = if a == b then Ok Bot else Error TypeError "Error"
ruleNotEilm forms _  = Error TypeError ("Not eliminaton takes 2 argument not " ++ show (List.length forms) ++".")

ruleBottomElim:: [Arg] -> Formula -> Result Formula
ruleBottomElim [ArgForm Bot] r = Ok r
ruleBottomElim [_a] _ = Error TypeError "Argument needs to be bottom"
ruleBottomElim forms _  = Error TypeError ("Bottom eliminaton takes 1 argument not " ++ show (List.length forms) ++".")

ruleNotNotIntro:: [Arg] -> Formula -> Result Formula
ruleNotNotIntro [ArgForm a] _ = Ok (Not (Not a))
ruleNotNotIntro forms _  = Error TypeError ("Not not introduction takes 1 argument not " ++ show (List.length forms) ++".")
ruleNotNotElim:: [Arg] -> Formula -> Result Formula
ruleNotNotElim [ArgForm (Not (Not a))] _ = Ok a
ruleNotNotElim [_a] _ = Error TypeError "Argument needs to be not not"
ruleNotNotElim forms _  = Error TypeError ("Not not eliminaton takes 1 argument not " ++ show (List.length forms) ++".")

ruleMT:: [Arg] -> Formula -> Result Formula
ruleMT [ArgForm (If a b), ArgForm (Not c)] r@(Not d) = if b == c && a==d then Ok r else Error TypeError "Arguments did not match"
ruleMT forms _  = Error TypeError ("MT takes 2 argument not " ++ show (List.length forms) ++".")

rulePBC:: [Arg] -> Formula -> Result Formula
rulePBC [ArgProof (Proof [Not a] Bot)] _ = Ok a
rulePBC forms _  = Error TypeError ("PBC eliminaton takes 1 argument not " ++ show (List.length forms) ++".")

ruleLEM:: [Arg] -> Formula -> Result Formula
ruleLEM [] r@(Or a (Not b)) = if a == b then Ok r else Error TypeError "Arguments did not match"
ruleLEM forms _  = Error TypeError ("LEM takes 0 argument not " ++ show (List.length forms) ++".")