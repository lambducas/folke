module Backend.Rules (
    ruleReiteration,
) where

import qualified Data.List as List
import Backend.Types



ruleReiteration:: [Formula] -> Result Formula
ruleReiteration [form] = Ok(form)
ruleReiteration forms  = Error TypeError ("Reiteration takes 1 argument not" ++ show (List.length forms) ++".")
