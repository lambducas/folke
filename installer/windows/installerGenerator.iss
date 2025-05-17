; Generates an installer for the proof editor

#define AppName "Folke"
#define EXEName "folke"
; #define AppVersion GetVersionNumbersString(AddBackslash(SourcePath) + "\releaseFiles\{#EXEName}.exe")
#define AppVersion "0.0.1"

[Setup]
AppName={#AppName}
AppVersion={#AppVersion}
WizardStyle=modern
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
; Since no icons will be created in "{group}", we don't need the wizard
; to ask for a Start Menu folder name:
; DisableProgramGroupPage=yes
UninstallDisplayIcon={app}\{#EXEName}.exe
Compression=lzma2
SolidCompression=yes
OutputDir=output\installer
OutputBaseFilename=folke_installer

DisableWelcomePage=no
; LicenseFile=license.txt

[Types]
Name: "full"; Description: "Full installation"
Name: "compact"; Description: "Compact installation"
Name: "custom"; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "program"; Description: "Program Files"; Types: full compact custom; Flags: fixed
Name: "desktopIcon"; Description: "Create Desktop Shortcut"; Types: full

[Files]
; Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme
Source: "output\releaseFiles\*"; DestDir: "{app}"; Flags: recursesubdirs

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\{#EXEName}.exe"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\{#EXEName}.exe"; Components: desktopIcon

[Run]
Filename: "{app}\{#EXEName}.exe"; Description: "Launch application"; Flags: postinstall nowait skipifsilent unchecked