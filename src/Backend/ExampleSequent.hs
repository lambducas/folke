module Backend.ExampleSequent (
    exampleSequent
) where

import Logic.Abs

exampleSequent :: Sequent
exampleSequent = Seq
    [ FormEq (Term (Ident "x") (Params [])) (Term (Ident "y") (Params []))
    , FormNot (FormEq (Term (Ident "a") (Params [])) (Term (Ident "b") (Params [])))
    ]
    (FormAnd (FormEq (Term (Ident "x") (Params [])) (Term (Ident "y") (Params [])))
             (FormNot (FormEq (Term (Ident "a") (Params [])) (Term (Ident "b") (Params [])))))
    [ StepPrem (FormEq (Term (Ident "x") (Params [])) (Term (Ident "y") (Params [])))
    , StepAssume (FormNot (FormEq (Term (Ident "a") (Params [])) (Term (Ident "b") (Params []))))
    , StepForm (Ident "someRule") [ArgLit 1] (FormAnd (FormEq (Term (Ident "x") (Params [])) (Term (Ident "y") (Params [])))
                                                      (FormNot (FormEq (Term (Ident "a") (Params [])) (Term (Ident "b") (Params [])))))
    ]
