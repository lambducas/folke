# Create installers

## Windows
Do this ***outside*** of WSL to ensure cabal builds an exe and not a Linux executable!

Install [Inno Setup](https://jrsoftware.org/isinfo.php) and make sure the it's added to the PATH.

Run the setup script
```bash
cd installer/windows
.\create_installer.sh
```

## Mac OS
Run the setup script
```bash
cd installer/macos
sh create_installer.sh
```

## Linux
*To be done*