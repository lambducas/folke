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
ruleReiteration forms          _ = Error TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++".")

ruleAndIntro:: [Arg] -> Formula -> Result Formula
ruleAndIntro [ArgForm a, ArgForm b] _ = Ok (And a b) 
ruleAndIntro [_, ArgForm _]         _ = Error TypeError "Argument 1 needs to be a formula."
ruleAndIntro [_, _]                 _ = Error TypeError "Argument 2 needs to be a formula."
ruleAndIntro forms                  _  = Error TypeError ("Rule takes 2 arguments not " ++ show (List.length forms) ++".")

ruleAndElimLeft::  [Arg] -> Formula -> Result Formula
ruleAndElimLeft [ArgForm (And l _)] _ = Ok l
ruleAndElimLeft [ArgForm _]         _ = Error TypeError "Argument needs to be a and formula."
ruleAndElimLeft [_]                 _ = Error TypeError "Argument needs to be a formula."
ruleAndElimLeft forms               _  = Error TypeError ("Rule takes 1 arguments not " ++ show (List.length forms) ++".")

ruleAndElimRight:: [Arg] -> Formula -> Result Formula
ruleAndElimRight [ArgForm (And _ r)] _ = Ok r
ruleAndElimRight [ArgForm _]         _ = Error TypeError "Argument needs to be a and formula."
ruleAndElimRight [_]                 _ = Error TypeError "Argument needs to be a formula."
ruleAndElimRight forms               _ = Error TypeError ("Rule takes 1 arguments not " ++ show (List.length forms) ++".")

ruleOrIntroLeft:: [Arg] -> Formula -> Result Formula
ruleOrIntroLeft [ArgForm a]  r@(Or b _) = if a == b then Ok r else Error TypeError "Argument 1 did not match left hand side of or."
ruleOrIntroLeft [ArgForm _]           _ = Error TypeError "Conclusion needs to be a or formula."
ruleOrIntroLeft [_]            (Or _ _) = Error TypeError "Argument needs to be a formula."
ruleOrIntroLeft forms                 _ = Error TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++".")

ruleOrIntroRight:: [Arg] -> Formula -> Result Formula
ruleOrIntroRight [ArgForm a]  r@(Or _ b) = if a == b then Ok r else Error TypeError "Argument 1 did not match right hand side of or."
ruleOrIntroRight [ArgForm _]           _ = Error TypeError "Conclusion needs to be a or formula."
ruleOrIntroRight [_]            (Or _ _) = Error TypeError "Argument needs to be a formula."
ruleOrIntroRight forms                 _ = Error TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++".")

ruleOrEilm:: [Arg] -> Formula -> Result Formula
ruleOrEilm [ArgForm (Or a b), ArgProof (Proof [p1] c1), ArgProof (Proof [p2] c2)] _ = if a == p1 then
    if b == p2 then 
        if c1 == c2 
            then Ok c1 
            else Error TypeError "The conclusions of the two proofs did not match."
        else Error TypeError "The premise of the second proof did not match the right hand side of the or statement."
    else Error TypeError "The premise of the second proof did not match the left hand side of the or statement."
ruleOrEilm [ArgForm (Or _ _), ArgProof _, _] _ = Error TypeError  "Argument 3 need to be a proof."
ruleOrEilm [ArgForm (Or _ _), _, _]          _ = Error TypeError  "Argument 2 need to be a proof."
ruleOrEilm [ArgForm _, _, _]                 _ = Error TypeError  "Argument 1 need to be a or formula."
ruleOrEilm [_, _, _]                         _ = Error TypeError  "Argument 1 need to be a formula."
ruleOrEilm forms                             _ = Error TypeError ("Rule takes 3 argument not " ++ show (List.length forms) ++".")

ruleIfIntro:: [Arg] -> Formula -> Result Formula
ruleIfIntro [ArgProof (Proof [a] b)] _ = Ok (If a b)
ruleIfIntro [_]                      _ = Error TypeError "Argument 1 need to be a proof."
ruleIfIntro forms                    _ = Error TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++".")

ruleIfEilm:: [Arg] -> Formula -> Result Formula
ruleIfEilm [ArgForm a, ArgForm (If b c)] r = if a == b then 
    if c == r 
        then Ok r
        else Error TypeError "Conclusions did not match"
    else Error TypeError "Premise did not match argument 1"
ruleIfEilm [ArgForm _, ArgForm _] _ = Error TypeError "Argument 2 need to be a if then formula."
ruleIfEilm [ArgForm _, _]         _ = Error TypeError "Argument 2 need to be a formula."
ruleIfEilm [_        , _]         _ = Error TypeError "Argument 1 need to be a formula."
ruleIfEilm forms                  _ = Error TypeError ("Rule takes 2 argument not " ++ show (List.length forms) ++".")

ruleNotIntro:: [Arg] -> Formula -> Result Formula
ruleNotIntro [ArgProof (Proof [a] Bot)] _ = Ok (Not a)
ruleNotIntro [ArgProof (Proof [_] _ )]  _ = Error TypeError "Argument 1 need to be a proof with the conclusion of bot."
ruleNotIntro [_]                        _ = Error TypeError "Argument 1 need to be a proof."
ruleNotIntro forms                      _  = Error TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++".")
ruleNotEilm:: [Arg] -> Formula -> Result Formula
ruleNotEilm [ArgForm a, ArgForm (Not b)] _ = if a == b then Ok Bot else Error TypeError "Argument 2 is not the negation of argument 1."
ruleNotEilm [ArgForm _, ArgForm _]       _ = Error TypeError "Argument 2 need to be a not formula."
ruleNotEilm [ArgForm _, _]               _ = Error TypeError "Argument 2 need to be a formula."
ruleNotEilm [_, _]                       _ = Error TypeError "Argument 1 need to be a formula."
ruleNotEilm forms                        _  = Error TypeError ("Rule takes 2 argument not " ++ show (List.length forms) ++".")

ruleBottomElim:: [Arg] -> Formula -> Result Formula
ruleBottomElim [ArgForm Bot] r = Ok r
ruleBottomElim [ArgForm _]   _ = Error TypeError "Argument 1 needs to be a bottom formula."
ruleBottomElim [_]           _ = Error TypeError "Argument 1 needs to be a formula."
ruleBottomElim forms         _ = Error TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++".")

ruleNotNotIntro:: [Arg] -> Formula -> Result Formula
ruleNotNotIntro [ArgForm a] _ = Ok (Not (Not a))
ruleNotNotIntro [_]         _ = Error TypeError "Argument 1 needs to be a formula."
ruleNotNotIntro forms       _  = Error TypeError ("Not not introduction takes 1 argument not " ++ show (List.length forms) ++".")
ruleNotNotElim:: [Arg] -> Formula -> Result Formula
ruleNotNotElim [ArgForm (Not (Not a))] _ = Ok a
ruleNotNotElim [ArgForm _]             _ = Error TypeError "Argument 1 needs to be a not not formula"
ruleNotNotElim [_]                     _ = Error TypeError "Argument 1 needs to be a formula"
ruleNotNotElim forms                   _ = Error TypeError ("Rule takes 1 argument not " ++ show (List.length forms) ++".")

ruleMT:: [Arg] -> Formula -> Result Formula
ruleMT [ArgForm (If a b), ArgForm (Not c)] _ = if b == c then Ok (Not a) else Error TypeError "Conclusion in argument 1 did not match argument 2"
ruleMT [ArgForm (If a b), ArgForm _]       _ = Error TypeError "Argument 2 need to be a not formula."
ruleMT [ArgForm (If a b), _]               _ = Error TypeError "Argument 2 need to be a formula."
ruleMT [ArgForm _, _ ]                     _ = Error TypeError "Argument 1 need to be a if then formula."
ruleMT [_, _]                              _ = Error TypeError "Argument 1 need to be a formula."
ruleMT forms                               _  = Error TypeError ("Rule takes 2 argument not " ++ show (List.length forms) ++".")

rulePBC:: [Arg] -> Formula -> Result Formula
rulePBC [ArgProof (Proof [Not a] Bot)] _ = Ok a
rulePBC [ArgProof (Proof [Not _] _)]   _ = Error TypeError "Conclusion in argument 1 need to be bot formula."
rulePBC [ArgProof (Proof [_] _)]       _ = Error TypeError "Premise in argument 1 need to be not formula."
rulePBC [_]                            _ = Error TypeError "Argument 1 need to be a proof."
rulePBC forms                          _  = Error TypeError ("Rule eliminaton takes 1 argument not " ++ show (List.length forms) ++".")

ruleLEM:: [Arg] -> Formula -> Result Formula
ruleLEM [] r@(Or a (Not b)) = if a == b then Ok r else Error TypeError "Right hand side is not the negation of the left hand side."
ruleLEM []                _ = Error TypeError "The conclusion of must be an or statement."
ruleLEM forms             _ = Error TypeError ("Rule takes 0 argument not " ++ show (List.length forms) ++".")