{-# LANGUAGE InstanceSigs #-}

module Backend.Types (
    -------------------------------------
    -- Core data types for formal logic
    -------------------------------------
    -- | Logic elements
    Term(..),
    Predicate(..),
    Formula(..),
    Proof(..),
    
    -- | Rule system
    Arg(..),
    UDefRule(..),
    
    -------------------------------------
    -- Error handling
    -------------------------------------
    -- | Result type
    Result(..),
    
    -- | Error types
    Error(..),
    ErrorKind(..),
    
    -- | Warning system
    Warning(..),
    WarningKind(..),
    Severity(..),
    
    -------------------------------------
    -- Environment
    -------------------------------------
    Env(..),
    Ref(..),
    IDType(..),
    
    -------------------------------------
    -- Type aliases
    -------------------------------------
    Terms,
    Refs,
    
    -------------------------------------
    -- Helper functions
    -------------------------------------
    -- | Formula manipulation
    occursFree,
    substitute,
    
    -- | Error creation
    createRuleArgError,
    createArgCountError,
    createRuleConcError,
    createTypeError,
    createReferenceError,
    createRuleNotFoundError,
    createUnknownError,
    createMismatchedFormulaError,
    createSyntaxError
) where

import Data.Map as Map (Map)
import Data.Maybe (listToMaybe)

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
data Severity = Hint | Low | Medium | High deriving (Eq, Ord, Show)

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
    refs       :: Map Ref ArgTup,  -- ^ References to labeled steps in the proof
    rules      :: Map String (Env -> [ArgTup] -> Formula -> Result Formula), -- ^ Built-in inference rules
    user_rules :: Map String UDefRule, -- ^ User-defined rules
    pos        :: Refs,            -- ^ Current position in the proof (for error reporting)
    rule       :: String,          -- ^ Current rule being applied
    bound      :: Map String (),   -- ^ Variables that are bound in the current scope
    ids        :: Map String IDType -- ^ Mapping of identifiers to their types
}

----------------------------------------------------------------------
-- Type class instances
----------------------------------------------------------------------

instance Show Term where
    show (Term name []) = name
    show (Term name terms) = name ++ "(" ++ show terms ++ ")"

instance Eq Term where
    Term a as == Term b bs = a == b && as == bs

instance Show Predicate where
    show (Predicate name []) = name
    show (Predicate name terms) = name ++ "(" ++ show terms ++ ")"

instance Eq Predicate where
    (==) :: Predicate -> Predicate -> Bool
    Predicate a as == Predicate b bs = a == b && as == bs

instance Show Formula where
    show (Pred a) = show a
    show (And a b) = show a ++ " ∧ " ++ show b
    show (Or a b) = show a ++ " ∨ " ++ show b
    show (Impl a b) = show a ++ " → " ++ show b
    show (Eq a b) = show a ++ " = " ++ show b
    show (All x a) = "∀" ++ show x ++ " " ++ show a
    show (Some x a) = "∃" ++ show x ++ " " ++ show a
    show (Not a) = "¬" ++ show a
    show Bot = "⊥"
    show Nil = "Nil"

instance Eq Formula where
    Pred a == Pred b = a == b
    And a1 a2 == And b1 b2 = a1 == b1 && a2 == b2
    Or a1 a2 == Or b1 b2 = a1 == b1 && a2 == b2
    Impl a1 a2 == Impl b1 b2 = a1 == b1 && a2 == b2
    Eq a1 a2 == Eq b1 b2 = a1 == b1 && a2 == b2

    All x a == All y b =
        -- If variables are the same, just compare subformulas
        (x == y && a == b) ||
        -- check x does not occur as free in b then substitute and check
        (not (occursFree x b) && a == substitute y x b)

    Some x a == Some y b =
        -- If variables are the same, just compare subformulas
        (x == y && a == b) ||
        (not (occursFree x b) && a == substitute y x b)

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
           (case warnLocation w of
              Just loc -> " at " ++ show loc
              Nothing -> "") ++
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
    -- | Map a function over the result value
    fmap :: (a -> b) -> Result a -> Result b
    fmap f (Ok warns x) = Ok warns (f x)
    fmap _ (Err warns env err) = Err warns env err

instance Applicative Result where
    -- | Lift a value into a successful result
    pure :: a -> Result a
    pure = Ok []
    
    -- | Apply a function in a Result to a value in a Result
    (<*>) :: Result (a -> b) -> Result a -> Result b
    (Ok warns1 f) <*> (Ok warns2 x) = Ok (warns1 ++ warns2) (f x)
    (Err warns env err) <*> _ = Err warns env err
    _ <*> (Err warns env err) = Err warns env err

instance Monad Result where
    -- | Return a value in a successful result
    return :: a -> Result a
    return = pure
    
    -- | Bind a computation to a function that returns a Result
    (>>=) :: Result a -> (a -> Result b) -> Result b
    (Ok warns x) >>= f = case f x of
        Ok newWarns y -> Ok (warns ++ newWarns) y
        Err newWarns env err -> Err (warns ++ newWarns) env err
    (Err warns env err) >>= _ = Err warns env err

----------------------------------------------------------------------
-- Helper functions for formulas
----------------------------------------------------------------------

-- | Check if a variable occurs free in a formula (not bound by a quantifier)
occursFree :: Term -> Formula -> Bool
occursFree v@(Term name []) formula = case formula of
    Pred (Predicate _ terms) -> v `elem` terms
    And f1 f2 -> occursFree v f1 || occursFree v f2
    Or f1 f2 -> occursFree v f1 || occursFree v f2
    Impl f1 f2 -> occursFree v f1 || occursFree v f2
    Eq t1 t2 -> v == t1 || v == t2
    All x f -> v /= x && occursFree v f
    Some x f -> v /= x && occursFree v f
    Not f -> occursFree v f
    Bot -> False
    Nil -> False
occursFree _ _ = False

-- | Substitute a term for a variable in a formula
substitute :: Term -> Term -> Formula -> Formula
substitute old new formula = case formula of
    Pred (Predicate name terms) ->
        Pred (Predicate name (map (\t -> if t == old then new else t) terms))
    And f1 f2 -> And (substitute old new f1) (substitute old new f2)
    Or f1 f2 -> Or (substitute old new f1) (substitute old new f2)
    Impl f1 f2 -> Impl (substitute old new f1) (substitute old new f2)
    Eq t1 t2 -> Eq (if t1 == old then new else t1) (if t2 == old then new else t2)
    All x f -> if x == old then All x f else All x (substitute old new f)
    Some x f -> if x == old then Some x f else Some x (substitute old new f)
    Not f -> Not (substitute old new f)
    Bot -> Bot
    Nil -> Nil

----------------------------------------------------------------------
-- Error creation functions
----------------------------------------------------------------------

-- | Create an error for an invalid rule argument
createRuleArgError :: Env -> Integer -> String -> Error
createRuleArgError env argNum message = Error {
  errLocation = listToMaybe (pos env),
  errKind = RuleArgError argNum message,
  errMessage = "Invalid argument to rule",
  errContext = Just $ "Problem with argument #" ++ show argNum,
  errSuggestions = ["Check that you're using the correct formula type"]
}

-- | Create an error for incorrect number of arguments to a rule
createArgCountError :: Env -> Integer -> Integer -> Error
createArgCountError env actual expected = Error {
  errLocation = listToMaybe (pos env),
  errKind = RuleArgCountError actual expected,
  errMessage = "Incorrect number of arguments",
  errContext = Just $ "Expected " ++ show expected ++ " arguments, got " ++ show actual,
  errSuggestions = ["Check that you're using the correct rule for your situation"]
}

-- | Create an error for an invalid rule conclusion
createRuleConcError :: Env -> String -> Error
createRuleConcError env message = Error {
  errLocation = listToMaybe (pos env),
  errKind = RuleConcError message,
  errMessage = "Invalid conclusion for rule application",
  errContext = Just message,
  errSuggestions = ["Check that your conclusion matches what the rule is expected to produce"]
}

-- | Create a type mismatch error
createTypeError :: Env -> String -> Error
createTypeError env message = Error {
  errLocation = listToMaybe (pos env),
  errKind = TypeError message,
  errMessage = "Type mismatch in proof",
  errContext = Just message,
  errSuggestions = ["Ensure you're using the correct type of expressions"]
}

-- | Create an error for an invalid reference
createReferenceError :: Env -> Ref -> String -> Error
createReferenceError env ref message = Error {
  errLocation = Just ref,  -- Use the reference itself as the location
  errKind = ReferenceError ref message,
  errMessage = "Invalid reference in proof",
  errContext = Just $ "Problem with reference " ++ show ref,
  errSuggestions = ["Check that the reference exists and is accessible"]
}

-- | Create an error for a rule that wasn't found
createRuleNotFoundError :: Env -> String -> Error
createRuleNotFoundError env ruleName = Error {
  errLocation = listToMaybe (pos env),
  errKind = RuleNotFoundError ruleName,
  errMessage = "Rule not found: " ++ ruleName,
  errContext = Just $ "The rule '" ++ ruleName ++ "' is not defined",
  errSuggestions = [
    "Check for typos in the rule name",
    "Use one of the predefined rules",
    "Define this rule as a user-defined rule first"
  ]
}

-- | Create a general error for unexpected situations
createUnknownError :: Env -> String -> Error
createUnknownError env message = Error {
  errLocation = listToMaybe (pos env),
  errKind = UnknownError message,
  errMessage = "Unexpected error occurred",
  errContext = Just message,
  errSuggestions = []
}

-- | Create an error for formulas that don't match
createMismatchedFormulaError :: Env -> Formula -> Formula -> Error
createMismatchedFormulaError env expected actual = Error {
  errLocation = listToMaybe (pos env),
  errKind = MismatchedFormula expected actual,
  errMessage = "Formulas don't match",
  errContext = Just $ "Expected: " ++ show expected ++ "\nActual: " ++ show actual,
  errSuggestions = ["Verify that your formula matches the required pattern for this rule"]
}

-- | Create an error for syntax problems in the proof
createSyntaxError :: Env -> String -> Error
createSyntaxError env message = Error {
    errLocation = listToMaybe (pos env),
    errKind = SyntaxError message,
    errMessage = "Syntax error in proof",
    errContext = Just $ "Parser error: " ++ message,
    errSuggestions = [
        "Check for missing parentheses or brackets",
        "Ensure all formulas are properly terminated",
        "Look for typos in predicate or variable names"
    ]
}
