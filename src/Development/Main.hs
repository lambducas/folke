import System.Directory
import Logic.Par
import qualified Logic.Abs as Abs
import Frontend.Parse
import Frontend.Types
import Data.Text (pack, intercalate, unpack)
import TextShow (showt)
import System.FilePath ((</>), takeBaseName, (<.>))

main :: IO ()
main = do
  putStrLn "Converting proofs..."
  convertDir "./test/proofs/good"
  putStrLn "Done."

  return ()

convertDir :: FilePath -> IO ()
convertDir dir = do
  filePaths <- listDirectory dir
  mapM_ (\f -> convertFile (dir </> f)) filePaths

convertFile :: FilePath -> IO ()
convertFile filePath = do
  let fileName = takeBaseName filePath
  cont <- readFile filePath
  case pSequent (myLexer cont) of
    Left _err -> return ()
    Right beSeq -> do
      let feSeq = convertSeq beSeq
      let jsonText = parseProofToJSON feSeq
      let jsonString = unpack jsonText
      writeFile ("./myProofs/exams/converted-" </> fileName <.> "json") jsonString
      putStrLn ("Wrote " <> fileName)

convertSeq :: Abs.Sequent -> FESequent
convertSeq (Abs.Seq premises conclusion proof) = FESequent (map convertForm premises) (convertForm conclusion) (convertProof proof)
  where
    convertForm (Abs.FormNot a) = "!" <> getOutput a
      where
        getOutput form = case form of
          Abs.FormPred _ -> c
          Abs.FormNot _ -> c
          Abs.FormBot -> c
          Abs.FormPar _ -> c
          _ -> p
          where c = convertForm form; p = "(" <> c <> ")"

    convertForm (Abs.FormAnd a b) = getOutput a <> " & " <> getOutput b
      where
        getOutput form = case form of
          Abs.FormImpl _ _ -> p
          _ -> c
          where c = convertForm form; p = "(" <> c <> ")"

    convertForm (Abs.FormOr a b) = getOutput a <> " | " <> getOutput b
      where
        getOutput form = case form of
          Abs.FormImpl _ _ -> p
          _ -> c
          where c = convertForm form; p = "(" <> c <> ")"

    convertForm (Abs.FormImpl a b) = convertForm a <> " -> " <> convertForm b
    convertForm (Abs.FormPred (Abs.Pred (Abs.Ident i) params)) = pack i <> convertParams params
    convertForm (Abs.FormPar a) = "(" <> convertForm a <> ")"
    convertForm (Abs.FormEq a b) = convertTerm a <> "=" <> convertTerm b
    convertForm (Abs.FormAll (Abs.Ident i) a) = "all " <> pack i <> " " <> convertForm a
    convertForm (Abs.FormSome (Abs.Ident i) a) = "some " <> pack i <> " " <> convertForm a
    convertForm Abs.FormBot = "bot"
    convertForm Abs.FormNil = ""

    convertProof (Abs.Proof proofElems) = concatMap convertProofElem proofElems
    convertProofElem (Abs.ProofElem _labels step) = convertStep step

    convertStep Abs.StepNil = [Line "" "" 0 []]
    convertStep (Abs.StepPrem _form) = []
    convertStep (Abs.StepAssume form) = [Line (convertForm form) "assume" 0 []]
    convertStep (Abs.StepProof pf) = [SubProof (convertProof pf)]
    convertStep (Abs.StepForm (Abs.Ident i) args form) = [Line (convertForm form) (pack i) (length args) (map convertArg args)]
    convertStep (Abs.StepFresh (Abs.Ident i)) = [Line (pack i) "fresh" 0 []]

    convertTerm (Abs.Term (Abs.Ident i) params) = pack i <> convertParams params

    convertParams (Abs.Params []) = ""
    convertParams (Abs.Params ts) = "(" <> intercalate ", " (map convertTerm ts) <> ")"

    convertArg (Abs.ArgLine i) = showt i
    convertArg (Abs.ArgRange a b) = showt a <> "-" <> showt b
    convertArg (Abs.ArgTerm t) = convertTerm t
    convertArg (Abs.ArgForm t f) = convertTerm t <> ":=" <> convertForm f