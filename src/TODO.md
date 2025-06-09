# Todo list

## General
- [ ] Fix exam_191029_3b.folke

- [ ] Test frontend to improve UX
- [ ] Fix crappy dependency management
- [ ] Huge cleanup to prepare public repo

- [x] Write help guide

## Backend
- [ ] Custom rules
- [ ] Better error messages
- [x] Add warning sensitivty using our Severity type
- [ ] Package backend with arbitrary frontend (DSL library for proof editor backend)

- [x] Empty lines should not be an error if it's the last line of the proof (if the proof is unfinished the error should be "unfinished proof" and not "empty step")
- [x] Fix bug empty last line
- [x] Improve warning system
- [x] Alpha-equivalence formulas
- [x] Update test system for JSON
- [x] Add checks for side conditions for rules.

## Frontend
- [ ] Fix full Markdown support
- [ ] Fix PDF export on Windows
- [ ] Fix better keyboard shortcuts
- [ ] Ability to rename proof-files

- [x] ~~Pressing left side of menubar dropdown does not close it~~
- [x] ~~It is possible to open the same file in seperate tabs if the working directory changes~~
- [x] Pasting text on windows crashes the program (known problem with Haskell-version of SDL2)
- [x] ~~`Open in file explorer` does not work in windows or mac build~~
- [x] ~~Windows file dialog does not show allowed files as ".folke" but random unicode characters~~
- [x] ~~Opening examples is not possible in windows build~~
- [x] ~~Window freezes when opening dialog on windows~~
- [x] ~~Line numbers become missaligned when there are errors/warnings taking up 2 or more lines~~
- [x] ~~Support user defined rules (derived rule from proof)~~
- [ ] ~~Toolbar instead of sidebar for symbol keypad??~~
- [x] ~~Auto-checker should only display critical errors and skip e.g empty line errors~~
- [x] ~~Send flags to backend when checking proof manually vs automatically~~
- [x] ~~Check proof on open~~
- [x] ~~When inserting a line after last line in subproof (using update ref mode), references to subproof don't update~~
- [x] ~~Fix suggestion dropdown (does not scroll to top + selected item needs to reset when outside range)~~
- [x] ~~Change button color in light theme~~
- [x] ~~Add support for up/down arrow on suggested input field~~
- [x] ~~Fix nativefiledialog on mac~~
- [x] ~~Fix menubar for better UX~~
- [x] ~~Add file search with shortcut `Ctrl+P` like VSCode~~
- [x] ~~Add buttons for inserting special characters~~
- [x] ~~When loading the files in the working directory, collapse all folders by default to prevent lag when opening large folders~~
- [x] ~~Tabs should be draggable so they can be reordered~~
- [x] ~~Add drag to resize on bottom bar where the error message is~~
- [x] ~~Allow user to open files that aren't in working directory by pressing `Ctrl+O` and selecting a single file~~
- [x] ~~Auto-check proof for errors~~
- [x] ~~Force user to save proofs as .json (or as a custom file ext.)~~
- [x] ~~When inserting a special character into a field, the cursor should not move to the end~~
- [x] ~~Ctrl Z support~~
- [x] ~~Allow the user to rearrange proof-lines by clicking and dragging (like previous bachelor thesis [Logan](https://odr.chalmers.se/server/api/core/bitstreams/e3cadeaa-efab-4e66-9a18-a41af5617d3e/content))~~
- [x] ~~Give suggestions on which rules to use when user is inputting rule~~
- [x] ~~User should be able to drag to resize 'File Explorer' and rule sidebar~~
- [x] ~~Display warnings and errors on the lines they appear on~~
- [x] ~~Update linenumber references when deleting or inserting line~~
- [x] ~~Fix focus not updating correcly when inserting new line/removing line~~
- [x] ~~When current working directory is removed, it should display an error instead of an empty directory in 'File Explorer'~~
- [x] ~~Allow the user to insert proof-lines above the current line (it's only possible to insert lines after the current line right now)~~
- [x] ~~Fix Markdown links on Windows~~
- [x] ~~`+ New line` should insert the line after the last line in same box as the last line~~
- [x] ~~Display all available rules nicely on the side~~
- [x] ~~Add textfield for every argument in rule instead of brackets in single textfield~~
- [x] ~~Catch syntax errors before backend parser so we don't get syntax errors that aren't helpful for the user~~
- [x] ~~Keybinding for check proof button~~
- [x] ~~Let the user hide both the 'File Explorer' and rule-lookup-guide like VSCode using Ctrl+B or GUI button~~
- [x] ~~Save currenly opened files to disk~~
- [x] ~~When creating a new proof, open a temporary unnamed proof file first and then when the user saves it, open a file dialog and save to disk~~
- [x] ~~All UI elements should scale not only text~~
