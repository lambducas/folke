#!/usr/bin/env bash

# Build
cd ../..
cabal build
cd ./installer/macos

# Create clean output directory
rm -rf ./output
mkdir ./output

# Copy app template
cp -R ./app_template ./output/app_template

# Create missing folders
mkdir -p ./output/app_template/Contents/MacOS
mkdir -p ./output/app_template/Contents/Resources

# Copy executable
cp ../../dist-newstyle/build/x86_64-osx/ghc-9.4.8/folke-0.1.0.0/x/folke/build/folke/folke ./output/app_template/Contents/MacOS

# Copy assets
cp -R ../../assets ./output/app_template/Contents/Resources/

# Create icon
mkdir ./output/icon.iconset

sips -Z 512 icon.png --out ./output/icon.iconset/icon_512.png
sips -Z 256 icon.png --out ./output/icon.iconset/icon_256.png
sips -Z 128 icon.png --out ./output/icon.iconset/icon_128.png
sips -Z 32  icon.png --out ./output/icon.iconset/icon_32.png
sips -Z 16  icon.png --out ./output/icon.iconset/icon_16.png

iconutil -c icns ./output/icon.iconset -o ./output/app_template/Contents/Resources/appIcon.icns

rm -rf ./output/icon.iconset

# Convert folder to app
mv ./output/app_template ./output/Folke.app

# Fix dylibs
cd ./output
install_name_tool -change /usr/local/opt/glew/lib/libGLEW.2.2.dylib @rpath/libGLEW.2.2.dylib Folke.app/Contents/MacOS/folke
install_name_tool -change /usr/local/opt/sdl2/lib/libSDL2-2.0.0.dylib @rpath/libSDL2-2.0.0.dylib Folke.app/Contents/MacOS/folke
install_name_tool -change /usr/local/opt/freetype/lib/libfreetype.6.dylib @rpath/libfreetype.6.dylib Folke.app/Contents/MacOS/folke
install_name_tool -change /usr/local/opt/libpng/lib/libpng16.16.dylib @loader_path/libpng16.16.dylib Folke.app/Contents/Frameworks/libfreetype.6.dylib
install_name_tool -add_rpath @executable_path/../Frameworks Folke.app/Contents/MacOS/folke
cd ..

# Create dmg
mkdir ./output/dmg_source
mv ./output/Folke.app ./output/dmg_source/

cd ./output

APP_NAME="Folke"
DMG_FILE_NAME="${APP_NAME}-Installer.dmg"
VOLUME_NAME="${APP_NAME} Installer"
SOURCE_FOLDER_PATH="dmg_source/"

CREATE_DMG=create-dmg

$CREATE_DMG \
  --volname "${VOLUME_NAME}" \
  --background "../dmg_background.png" \
  --window-pos 200 120 \
  --window-size 800 400 \
  --icon-size 100 \
  --icon "${APP_NAME}.app" 200 190 \
  --hide-extension "${APP_NAME}.app" \
  --app-drop-link 600 185 \
  "${DMG_FILE_NAME}" \
  "${SOURCE_FOLDER_PATH}"

cd ..

# Remove app
# rm -rf ./output/dmg_source