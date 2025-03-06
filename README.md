
# Proof Editor using Monomer & BNFC with Haskell

## Overview

This project is a proof editor built using the Monomer library for the frontend and BNFC for parsing logic expressions in Haskell. The editor allows users to create, edit, and verify logical proofs.

## Haskell Code Example

```haskell
\_ -> undefined
```

## Building the Project

To build the library and executable, run the following command:

```bash
cabal build
```

## Running the Project

To run the backend (standard output) and frontend GUI, use the following command:

```bash
cabal run bsc
```

## TODO

- Improve the semantics of the TypeChecker module (environment handling, form checking, non-exhaustiveness).
- Add an assets folder in the `Frontend/` directory.
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

## License?
