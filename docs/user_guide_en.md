# User Guide
A user guide for Folke. 

## Create new proof
Press `Ctrl+N` or go to `File > New Proof` to [create an empty proof](runinternalevent://CreateEmptyProof)

## Open proof
Press `Ctrl+O` or go to `File > Open File`. Then select the proof you want to open.

## Open example proof
Go to `File > Open Example`. Then select the example you want to open.

## Save proof
When a proof is edited a dot appears next to the name in its tab. Press `Ctrl+S` or

go to `File > Save File` to save the current file. The dot will disappear indicating

that the file has been saved.

## Write a proof
1. Open or create a new proof
2. **Premises:** Enter each premise on a seperate line by pressing `+ Premise`.
3. **Conclusion:** Enter the conclusion in the conclusion-field
4. **Proof:** Enter each statement and rule on seperate lines. Below is a list of helpful actions.

## Validate proof
Enable `Auto-validate proof` using the checkbox or press `Validate proof` or

`Ctrl+R` to check if your proof is correct.

## Warnings
A proof can be correct even with warnings. Warnings highlight unused lines,

duplicate lines, empty lines, etc. Toggle between showing all warnings,

some warnings and only severe warnings in the right must dropdown in the

toolbar.

## Helpful actions and shortcuts
* **Insert new line below:** Press `↓+` or `Enter` when the last argument-field is selected to insert a line below the current line.
* **Insert new line above:** Press `↑+` to insert a line above the current line.
* **Create subproof:** Press `→☐` or `Ctrl+Tab` to convert the current line to a subproof.
* **Undo subproof:** Press `☒` or `Ctrl+Shift+Tab` to remove the box around a subproof containing a single line.
* **Close subproof:** Press `⏎` or `Ctrl+Enter` to insert a line below the current subproof.
* **Other actions:** Right-click a line to see all available actions.

## Quick-type characters
Open the rule sidebar from `View > Toggle Rules Dictionary` to get a keypad with

a special characters. It is also possible to use a keyboard to write the characters for:
* **Negate**: ¬, !  ~
* **Implies**: → -> >
* **Conjunction**: ∧ & ^ * and con
* **Disjunction**: ∨ | + or dis
* **Contradiction**: ⊥ bot # XX
* **Universal quantifier**: ∀ all forall
* **Existential quantifier**: ∃ some exists
* **Subscript**: ₀ ₁ ₂ ₃ ₄ ₅ ₆ ₇ ₈ ₉ _0 _1 _2 _3 _4 _5 _6 _7 _8 _9

Extra shortcuts can be enabled in preferences.

## Export proof to Latex
Navigate to `File` in the menu bar and press `Export to LaTeX` or `Export to LaTeX and PDF`

to export your proof as LaTeX code or as a ready-to-use PDF.

### Export to LaTeX with PDF compilation
Automatically compiles the LaTeX source code into a PDF document. Creates both

.tex source file and .pdf file at your chosen location.

Make sure you have pdflatex installed on your system:
  - On Ubuntu/Debian: `sudo apt-get install texlive-latex-base`
  - On macOS with Homebrew: `brew install basictex`
  - On Windows: Install MiKTeX or TeX Live

## Open settings/preferences
Press `Ctrl+Shift+P`, `View > Open preferences` or press the cog icon in the left sidebar.

## Advanced file management
It is possible to work with multiple files at the same time by opening a folder as working directory.

### Open File Explorer
Press `Ctrl+B` or `View > Toggle File Explorer` to toggle the file explorer sidebar.

### Open rules dictionary
Press `View > Toggle Rules Dictionary` to toggle the rules dictionary.

### Set working directory
Press `Open folder` in the File Explorer or `File > Set Working Directory` and select a folder.

### Find file
Press `Ctrl+P` or `View > Search for File` and enter a search query to find a file in

the current working directory. Note that nothing will happen if no working directory is set.
