## TODO
### Backend
- [x] Empty lines should not be an error if it's the last line of the proof (if the proof is unfinished the error should be "unfinished proof" and not "empty step")
- [ ] Improve warning system
- [ ] Warn on unused steps (reference counting)
- [ ] Custom rules
- [ ] Better messages
- [ ] JSON proofs
- [x] Alpha-equivalence formulas
- [x] Update test system for JSON
- [x] Add checks for side conditions for rules.
#### And if ambition enables:
- [ ] Suggest next step of proof
- [ ] JSON node for error
#### Suggestions for new rule names
| Old name  | New/secondary names      |
|-----------|--------------------------|
| copy      | COPY, C, REITERATION, R  |
| AndI      | &I, ∧I                   |
| AndEL     | &EL, ∧EL                 |
| AndER     | &ER, ∧ER                 |
| -         | &E, ∧E                   |
| OrIL      | \|IL, ∨IL                |
| OrIR      | \|IR, ∨IR                |
| -         | \|I, ∨I                  |
| OrE       | \|E, ∨E                  |
| IfI       | ->I, →I                  |
| IfE       | ->E, →E                  |
| ImplI     | ->I, →I                  |
| ImplE     | ->E, →E                  |
| NotI      | !I, ¬I                   |
| NotE      | !E, ¬E                   |
| BotE      | botE, #E, ⊥E             |
| NotNotI   | !!I, ¬¬I                 |
| NotNotE   | !!E, ¬¬E                 |
| MT        | MT                       |
| PBC       | PBC                      |
| LEM       | LEM                      |
| EqI       | =I                       |
| EqE       | =E                       |
| AllE      | forallE, ∀E              |
| AllI      | forallI, ∀I              |
| SomeE     | existsE, ∃E              |
| SomeI     | existsI, ∃I              |

### Frontend
- [ ] Fix full Markdown support
- [ ] Ctrl Z support
- [ ] Fix nativefiledialog on mac
- [ ] Fix menubar for better UX
- [ ] When loading the files in the working directory, collapse all folders by default to prevent lag when opening large folders
- [ ] Auto-check proof for errors (only display critical errors and skip e.g empty line errors)
- [ ] When inserting a line after last line in subproof (using update ref mode), references to subproof don't update
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

### Other
- Test frontend to improve UX
- Write help guide
- Package backend with arbitrary frontend (library for proof editor backend)

## IDEAS
- Support for subscript in front end, nice if x_0 renders fancy.

## Build

To build the library, run the following command:

```bash
$ cabal build
```
## Run
To run the executable, run the following command:

```bash
$ cabal run
```

### Set up BNFC
To install BNFC and generate files, run:
```bash
$ cabal install BNFC
$ cabal install alex
$ cabal install happy
$ make
```

### Fix freetype2 not found
On Ubuntu, to fix `rejecting: nanovg:-stb_truetype (conflict: pkg-config package
freetype2-any, not found in the pkg-config database)` when building, run
```bash
sudo apt-get install libfreetype6-dev
```

## Setup nativefiledialog
```bash
sudo apt-get install libgtk-3-dev
# Should not be needed:
# cd nativefiledialog-hs/nativefiledialog/build/gmake_linux
# make config=release_x64
```

## Running the Project

To run the backend (standard output) and frontend GUI, use the following command to run the executable:

```bash
$ cabal run bsc
```

## Run tests

To run tests, use the following command:

```bash
$ cabal test
```

## License?

## Windows installation

1. Download GHCUP if its not already installed on windows
1. Start mingw64 terminal: `C:\ghcup\msys64\mingw64.exe`
1. Run all these commands in the mingw64 terminal
    ```bash
    $ pacman -S mingw-w64-x86_64-pkg-config
    $ pacman -S mingw-w64-x86_64-SDL2
    $ pacman -S mingw-w64-x86_64-freeglut
    $ pacman -S mingw-w64-x86_64-glew
    $ pacman -S mingw-w64-x86_64-freetype
    $ pacman -S make
    ```
1. Set up BNFC next. Run these commands in the normal windows command prompt:
    ```bash
    > ghcup run --mingw-path -- cabal install BNFC
    > ghcup run --mingw-path -- cabal install alex
    > ghcup run --mingw-path -- cabal install happy
    ```
    (`ghcup run --mingw-path` sets the PATH temporarily so cabal finds pkg-config)
1. Run make and expect it to partially fail saying either alex or happy not found
    ```bash
    > ghcup run --mingw-path -- make
    ```
1. Replace replace `happy` on line 6 with `C:\\cabal\\bin\\happy.exe` and `alex` on line 8 with `C:\\cabal\\bin\\alex.exe` in src/Makefile
1. Run ther makefile with and expect build errors
    ```bash
    > cd src
    \src> ghcup run --mingw-path -- make
    \src> cd ..
    >
    ```
1. Run in top directory to build project (run twice if you get permission error first time):
    ```bash
    > ghcup run --mingw-path -- cabal build
    ```
    but SDL2 will probably fail to build with `ld.lld: error: undefined symbol: __stack_chk_fail`
1. Run the project
    ```bash
    > ghcup run --mingw-path -- cabal build
    > cabal run bsc
    ```

### SDL2 build error solution
If sdl2 fails with `error: ld.lld: error: undefined symbol: __stack_chk_fail` (from [this](https://github.com/haskell-game/sdl2/issues/277#issuecomment-2283057736) github comment ❤🤗):
1. Download `SDL2-devel-2.30.6-mingw.zip` from [this repo](https://github.com/libsdl-org/SDL/releases/tag/release-2.30.6)
1. Unzip and place the folder `SDL2-2.30.6` in Downloads or somewhere else. Make sure it contains a folder called `x86_64-w64-mingw32` with subfolders: bin, include, lib and share
1. Run the following 3 commands in a mingw64 terminal (replace `your-user-name` with your actual username and make sure the first path of `cp` points to your downloaded folder)
    ```bash
    $ cd /c/ghcup/msys64
    $ cp /c/Users/your-user-name/Downloads/SDL2-2.30.6/x86_64-w64-mingw32/lib/* -r /mingw64/lib/
    $ cp /c/Users/your-user-name/Downloads/SDL2-2.30.6/x86_64-w64-mingw32/include/* -r /mingw64/include/
    $ cp /c/Users/your-user-name/Downloads/SDL2-2.30.6/x86_64-w64-mingw32/bin/* -r /mingw64/bin/
    ```
1. Run `ghcup run --mingw-path -- cabal build` again and it should work (it works on my machine 💀)

#### Useful links maybe
Getting cabal to find pkg-config: https://discourse.haskell.org/t/installing-a-library-with-c-dependencies-on-windows/8557
Old SDL2 versions: https://repo.msys2.org/mingw/mingw64/
Solve sdl2 build error: https://github.com/haskell-game/sdl2/issues/277#issuecomment-2283057736

<!-- SDL2-2.0.14 build files: https://github.com/msys2/MINGW-packages/tree/76df904503a525e3043462ebf65ab6377182a22a/mingw-w64-SDL2
Build with: `makepkg --syncdeps --skippgpcheck` in mingw64.exe in ghcup -->
