#!/bin/bash

# Create icon
ghcup run --mingw-path -- windres -i Icon.rc ../../Icon.o

# Build
cd ../..
ghcup run --mingw-path -- cabal build
cd ./installer/windows

# Create clean output directory
rm -rf ./output
mkdir ./output

# Create releaseFiles
mkdir ./output/releaseFiles

# Copy executable
cp ../../dist-newstyle/build/x86_64-windows/ghc-9.4.8/folke-0.1.0.0/x/folke/build/folke/folke.exe ./output/releaseFiles

# Copy assets
cp -R ../../assets ./output/releaseFiles

# Copy DLLs
source ./copyAllDLLsHere.sh

# Compile installer
iscc "./installerGenerator.iss"

# Remove temp files
rm -rf ./output/releaseFiles

# Wait to close
read -p "Press enter to continue"