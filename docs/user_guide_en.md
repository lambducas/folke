# Proof Editor User Guide
A quick guide to get you started

## Create your first proof
Begin by creating a proof-file:
1. Press `Ctrl+N` to create a proof
2. Enter a filename for your proof-file
3. Press `Enter` or "+ Create proof"

## Write your proof
### Premises
Enter each premise on a new line by pressing "+ Premise".

### Conclusion
Enter the conclusion in the conclusion-field

### Proof
0. You do not need to write the premises again as they are automatically added to your proof.
1. Enter the formula in the first input on line `1.` and the rule in the second input.
2. Press `Enter` or on "↓+" to insert a new line.

#### Create subproof
If you need to create a subproof below the current line, first insert a line and then convert it to a subproof
1. Press `Enter` or "↓+" to insert a new line.
2. Press `Ctrl+Tab` or "→☐" to convert the newly created line into a subproof.

#### Symbol table
<!-- | Name | Accepted Symbols |
|---|---|
| And | & |
| Or | \| |
| Implies | -> | -->

#### Rules

##### Keyboard Shortcuts

#### Exporting your proofs
##### Export to LaTeX
- Converts your proof into tex-typed source code with good formatting using ams and logicproof libraries
- In the save dialog you choose the save location

##### Export to LaTeX with PDF compilation
- Same as Export to LaTeX
- Automatically compiles the LaTeX source code into a PDF document
- Creates both .tex source file and .pdf file at your chosen location
- Make sure you have pdflatex installed on your system:
  - On Ubuntu/Debian: `sudo apt-get install texlive-latex-base`
  - On macOS with Homebrew: `brew install basictex`
  - On Windows: Install MiKTeX or TeX Live

##### Benefits of PDF Export
- Get an immediately viewable document
- Easy to share with others
- Professional typesetting of mathematical formulas
- Ready for printing or submission
