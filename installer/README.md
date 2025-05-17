# Create installers

## Windows
Do this OUTSIDE of WSL to ensure cabal builds an exe and not a Linux executable!
1. Install [Inno Setup](https://jrsoftware.org/isinfo.php)
1. Build cabal following the windows build instructions above
1. Copy `bsc.exe` from `dist-newstyle` (somewhere inside build/..../bsc-project/x/....) to `installer/releaseFiles`
1. Copy `assets/` to to `installer/releaseFiles`
1. Move `copyAllDLLsHere.sh` to `installer/releaseFiles` and run the script. It should generate a dozen DLL's inside `installer/releaseFiles`
1. Build `installerGenerator.iss` with Inno Setup. A file called `mysetup.exe` should be generated in `installer/Setup output`
1. Distribute `mysetup.exe` to user however you like

## Mac OS
```bash
cabal build
cd installer/macos
sh create_installer.sh
```