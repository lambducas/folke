## For now very empty
```haskell
\_ -> undefined
```
## Start monomer view which does nothing at the moment
```bash
cabal build
cabal run monomergui
```

### Fix freetype2 not found
On Ubuntu, to fix `rejecting: nanovg:-stb_truetype (conflict: pkg-config package
freetype2-any, not found in the pkg-config database)` when building, run
```bash
sudo apt-get install libfreetype6-dev
```