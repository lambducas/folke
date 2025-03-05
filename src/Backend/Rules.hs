module Backend.Rules (
    applyRule,
    Rule(..)
) where

import Parser.Logic.Abs

data Rule = MP | AndI | AndE1 | AndE2
    deriving (Eq, Show)

applyRule :: Rule -> [Form] -> Either String Form
applyRule MP [FormIf p q, p'] | p == p' = Right q
applyRule AndI [p, q] = Right (FormAnd p q)
applyRule AndE1 [FormAnd p _] = Right p
applyRule AndE2 [FormAnd _ q] = Right q
applyRule _ _ = Left "Invalid rule application"
