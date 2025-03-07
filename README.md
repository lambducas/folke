## TODO

- Improve the semantics of the TypeChecker module (environment handling, form checking, non-exhaustiveness).
- Add an assets folder in `Frontend/` and remove `assets/` in root directory.
- Write examples of backend-frontend communication.

## Directory Structure

```
bsc-project/
├── app/
│   ├── Example.hs
│   ├── Main.hs
├── src/
│   ├── Backend/
│   │   ├── ExampleSequent.hs
│   │   ├── Rules.hs
│   │   ├── TypeChecker.hs
│   ├── Frontend/
│   │   ├── Communication.hs
│   │   ├── Main.hs
│   ├── Parser/
│   │   ├── Logic/
│   │   │   ├── Abs.hs
│   ├── Shared/
│   │   ├── Messages.hs
├── test/
│   ├── Main.hs
├── CHANGELOG.md
├── LICENSE
├── README.md
├── bsc-project.cabal
```


# Proof Editor using Monomer & BNFC with Haskell

## Overview

This project is a proof editor built using the Monomer library for the frontend and BNFC for parsing logic expressions in Haskell. The editor allows users to create, edit, and verify logical proofs.

## Haskell

```haskell
\_ -> undefined
```

## Building the Project

To build the library and executable, run the following command:

```bash
$ cabal build
```

### Fix freetype2 not found
On Ubuntu, to fix `rejecting: nanovg:-stb_truetype (conflict: pkg-config package
freetype2-any, not found in the pkg-config database)` when building, run
```bash
sudo apt-get install libfreetype6-dev
```

## Running the Project

To run the backend (standard output) and frontend GUI, use the following command to run the executable:

```bash
$ cabal run bsc
```


## License?
