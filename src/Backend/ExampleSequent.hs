module Backend.ExampleSequent where

import Parser.Logic.Abs

exampleSequent :: Sequent
exampleSequent = Seq
    [ FormEq (Term (TermId "x") (Params [])) (Term (TermId "y") (Params []))
    , FormNot (FormEq (Term (TermId "a") (Params [])) (Term (TermId "b") (Params [])))
    ]
    (FormAnd (FormEq (Term (TermId "x") (Params [])) (Term (TermId "y") (Params [])))
             (FormNot (FormEq (Term (TermId "a") (Params [])) (Term (TermId "b") (Params [])))))
    [ StepPrem (FormEq (Term (TermId "x") (Params [])) (Term (TermId "y") (Params [])))
    , StepAssume (FormNot (FormEq (Term (TermId "a") (Params [])) (Term (TermId "b") (Params []))))
    , StepForm (RuleId "someRule") [ArgLit 1] (FormAnd (FormEq (Term (TermId "x") (Params [])) (Term (TermId "y") (Params [])))
                                                      (FormNot (FormEq (Term (TermId "a") (Params [])) (Term (TermId "b") (Params [])))))
    ]
