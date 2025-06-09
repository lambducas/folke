{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Types (
    -- Core data types
    Term(..),
    Predicate(..),
    Formula(..),
    Proof(..),
    Arg(..),
    UDefRule(..),
    
    -- Error handling
    Result(..),
    Error(..),
    ErrorKind(..),
    Warning(..),
    WarningKind(..),
    Severity(..),
    
    -- Environment
    Env(..),
    Ref(..),
    IDType(..),
    
    -- Type aliases
    Terms,
    Refs,
    Formulas,
    ArgTup
) where

import qualified Data.Map as Map
import qualified Data.List as List

----------------------------------------------------------------------
-- Type aliases
----------------------------------------------------------------------

-- | A list of terms
type Terms = [Term]

-- | A list of references
type Refs = [Ref]

-- | A list of formulas
type Formulas = [Formula]

-- | A tuple of argument index and argument
type ArgTup = (Integer, Arg)

----------------------------------------------------------------------
-- Core data types for logic
----------------------------------------------------------------------

-- | A term, either a variable or function application with arguments
data Term = Term String [Term] deriving Ord

-- | A predicate with a name and list of term arguments
data Predicate = Predicate String [Term] deriving Ord

-- | A logical formula in first-order logic
data Formula =
    Pred Predicate              -- ^ Predicate application
  | And Formula Formula         -- ^ Logical conjunction
  | Or Formula Formula          -- ^ Logical disjunction
  | Impl Formula Formula        -- ^ Logical implication
  | Eq Term Term                -- ^ Equality between terms
  | All Term Formula            -- ^ Universal quantification
  | Some Term Formula           -- ^ Existential quantification
  | Not Formula                 -- ^ Logical negation
  | Bot                         -- ^ Bottom (false)
  | Nil                         -- ^ Nil (used for incomplete formulas)

-- | A proof consisting of fresh variables, premises, and a conclusion
data Proof = Proof [Term] [Formula] Formula

-- | An argument to a rule application
data Arg = 
    ArgProof Proof                -- ^ A proof box as an argument 
  | ArgForm Formula               -- ^ A formula as an argument
  | ArgTerm Term                  -- ^ A term as an argument
  | ArgFormWith Term Formula      -- ^ A formula with a term variable (for substitution)

-- | A user-defined rule with input formulas and output formula
data UDefRule = UDefRule [Formula] Formula

----------------------------------------------------------------------
-- Error and Result system
----------------------------------------------------------------------

-- | Severity levels for warnings
data Severity = Low | Medium | High deriving (Eq, Ord, Show)

-- | Types of warnings that can be generated
data WarningKind =
    UnusedReference Ref                -- ^ A reference was declared but never used
  | IncompleteProof Formula Formula    -- ^ A proof is incomplete (expected vs actual)
  | PossibleSimplification String      -- ^ A proof step could be simplified
  | RedundantStep                      -- ^ A step in the proof is redundant
  | StyleIssue String                  -- ^ A style issue with the proof
  | GeneralWarning String              -- ^ A general warning
  deriving (Show)

-- | A warning with location, severity, and message information
data Warning = Warning {
  warnLocation :: Maybe Ref,        -- ^ Optional location of the warning
  warnSeverity :: Severity,         -- ^ Severity level of the warning
  warnKind :: WarningKind,          -- ^ Type of warning
  warnMessage :: String,            -- ^ Warning message
  warnSuggestion :: Maybe String    -- ^ Optional suggestion for fixing the warning
}

-- | Types of errors that can occur during proof verification
data ErrorKind =
  -- Basic error types
    TypeError String                  -- ^ Type mismatch in proof
  | SyntaxError String                -- ^ Syntax error in input
  | RuleNotFoundError String          -- ^ Referenced rule not found
  | RuleConcError String              -- ^ Error in rule conclusion
  | RuleArgError Integer String       -- ^ Error in rule argument
  | RuleArgCountError Integer Integer -- ^ Wrong number of arguments for rule

  -- Advanced error types
  | MismatchedFormula Formula Formula -- ^ Formula mismatch (expected vs actual)
  | ReferenceError Ref String         -- ^ Error with a reference
  | OutOfScopeReference Ref           -- ^ Reference is out of scope
  | RuleApplicationError String Formula -- ^ Error applying a rule to a formula
  | MalformedProof String             -- ^ Structurally invalid proof
  | IncompleteProofError Formula      -- ^ Proof doesn't reach required conclusion
  | EmptyFormula String               -- ^ Rule is provided, but the formula is empty
  | EmptyPremise Integer String       -- ^ Premise is empty
  | EmptyConclusion String            -- ^ Conclusion is empty
  | UnknownError String               -- ^ Unclassified error
  deriving (Show)

-- | A structured error with location, context, and suggestions
data Error = Error {
  errLocation :: Maybe Ref,          -- ^ Optional location of the error
  errKind :: ErrorKind,              -- ^ Type of error
  errMessage :: String,              -- ^ Error message
  errContext :: Maybe String,        -- ^ Additional context for the error
  errSuggestions :: [String]         -- ^ Possible fixes for the error
}

-- | Result type for computations that can fail with an error or succeed with warnings
data Result a = 
    Ok [Warning] a               -- ^ Success with optional warnings
  | Err [Warning] Env Error      -- ^ Failure with error, environment state, and optional warnings

----------------------------------------------------------------------
-- Environment
----------------------------------------------------------------------

-- | A reference to a position in a proof, either a line or range
data Ref = RefRange Integer Integer | RefLine Integer deriving (Eq, Ord)

-- | Type information for identifiers
data IDType = 
    IDTypeTerm Integer      -- ^ A term with arity
  | IDTypePred Integer      -- ^ A predicate with arity

-- | The environment for the proof system, containing all context needed for verification
data Env = Env {
    prems      :: Formulas,        -- ^ Premises/assumptions in the current scope
    depth      :: Integer,         -- ^ Current nesting depth of proofs
    fresh      :: Terms,           -- ^ Fresh variables in the current scope
    refs       :: Map.Map Ref ArgTup,  -- ^ References to labeled steps in the proof
    rules      :: Map.Map String (Env -> [ArgTup] -> Formula -> Result Formula), -- ^ Built-in inference rules
    user_rules :: Map.Map String UDefRule, -- ^ User-defined rules
    pos        :: Refs,            -- ^ Current position in the proof (for error reporting)
    rule       :: String,          -- ^ Current rule being applied
    bound      :: Map.Map String (),   -- ^ Variables that are bound in the current scope
    ids        :: Map.Map String IDType -- ^ Mapping of identifiers to their types
}

----------------------------------------------------------------------
-- Type class instances
----------------------------------------------------------------------

instance Show Term where
    show (Term name []) = name
    show (Term name terms) = name ++ "(" ++ (List.intercalate ", " [show term| term <- terms]) ++ ")"

instance Eq Term where
    Term a as == Term b bs = a == b && as == bs

instance Show Predicate where
    show (Predicate name []) = name
    show (Predicate name terms) = name ++ "(" ++ (List.intercalate ", " [show term| term <- terms]) ++ ")"

instance Eq Predicate where
    (==) :: Predicate -> Predicate -> Bool
    Predicate a as == Predicate b bs = a == b && as == bs

instance Show Formula where
  show :: Formula -> String
  show (Pred a) = show a
  show (Not a) = "¬" <> getOutput a
        where
          getOutput form = case form of
            Pred _ -> c
            Not _ -> c
            Bot -> c
            _ -> p
            where c = show form; p = "(" <> c <> ")"

  show (And a b) = getOutput a <> " ∧ " <> getOutput b
    where
      getOutput form = case form of
        Impl _ _ -> p
        _ -> c
        where c = show form; p = "(" <> c <> ")"

  show (Or a b) = getOutput a <> " ∨ " <> getOutput b
    where
      getOutput form = case form of
        Impl _ _ -> p
        _ -> c
        where c = show form; p = "(" <> c <> ")"

  show (Impl a b) = show a <> " → " <> show b
  show (Eq a b) = show a <> "=" <> show b
  show (All i a) = "∀" <> show i <> " " <> show a
  show (Some i a) = "∃" <> show i <> " " <> show a
  show Bot = "⊥"
  show Nil = "nothing"

-- | Deprecated
instance Eq Formula where
    Pred a == Pred b = a == b
    And a1 a2 == And b1 b2 = a1 == b1 && a2 == b2
    Or a1 a2 == Or b1 b2 = a1 == b1 && a2 == b2
    Impl a1 a2 == Impl b1 b2 = a1 == b1 && a2 == b2
    Eq a1 a2 == Eq b1 b2 = a1 == b1 && a2 == b2
    All x a == All y b = x == y && a == b 
    Some x a == Some y b = x == y && a == b
    Not a == Not b = a == b
    Bot == Bot = True
    Nil == Nil = True
    _ == _ = False

instance Show Proof where
    show (Proof [] premises conc) = show premises ++ " ⊢ " ++ show conc
    show (Proof terms premises conc) = show terms ++ show premises ++ " ⊢ " ++ show conc

instance Eq Proof where
    (==) :: Proof -> Proof -> Bool
    Proof terms1 prems1 conc1 == Proof terms2 prems2 conc2 = 
        terms1 == terms2 && prems1 == prems2 && conc1 == conc2

instance Show Arg where
    show (ArgProof p) = show p
    show (ArgForm f) = show f
    show (ArgTerm t) = show t
    show (ArgFormWith x phi) = "φ(" ++ show x ++ ") ≡ " ++ show phi

instance Show Ref where
    show (RefRange i j) = show i ++ "-" ++ show j
    show (RefLine i) = show i

instance Show Warning where
  show w = "[" ++ show (warnSeverity w) ++ "] " ++
          warnMessage w ++
          -- GUI already shows location
          --  (case warnLocation w of
          --     Just loc -> " at " ++ show loc
          --     Nothing -> "") ++
          maybe "" ("\nSuggestion: " ++) (warnSuggestion w)

instance Show Error where
  show e = show (errKind e) ++
           (case errLocation e of
              Just loc -> " at " ++ show loc
              Nothing -> "") ++
           ": " ++ errMessage e ++
           maybe "" ("\nContext: " ++) (errContext e) ++
           (if null (errSuggestions e)
             then ""
             else "\nSuggestions:\n" ++ unlines (map ("- " ++) (errSuggestions e)))

instance Functor Result where
    fmap f (Ok warns x) = Ok warns (f x)
    fmap _ (Err warns env err) = Err warns env err

instance Applicative Result where
    pure = Ok []
    (Ok warns1 f) <*> (Ok warns2 x) = Ok (warns1 ++ warns2) (f x)
    (Err warns env err) <*> _ = Err warns env err
    _ <*> (Err warns env err) = Err warns env err

instance Monad Result where
    return = pure
    (Ok warns x) >>= f = case f x of
        Ok newWarns y -> Ok (warns ++ newWarns) y
        Err newWarns env err -> Err (warns ++ newWarns) env err
    (Err warns env err) >>= _ = Err warns env err