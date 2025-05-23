# Create installers

## Windows
Do this ***outside*** of WSL to ensure cabal builds an exe and not a Linux executable!

Install [Inno Setup](https://jrsoftware.org/isinfo.php) and make sure that it's added to the PATH.

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

# References
1. https://gelisam.blogspot.com/2014/12/how-to-package-up-binaries-for.html
1. https://medium.com/design-bootcamp/5-steps-to-create-a-macos-app-icon-in-icns-format-a659f27f1a85
1. https://stackoverflow.com/questions/54455207/how-to-create-a-mac-os-app-bundle-with-chef-omnibus-in-dmg-installer/54461081#54461081
1. https://developer.apple.com/library/archive/documentation/CoreFoundation/Conceptual/CFBundles/BundleTypes/BundleTypes.html#//apple_ref/doc/uid/10000123i-CH101-SW1