#!/bin/bash

# Source:
# https://blog.rubenwardy.com/2018/05/07/mingw-copy-dlls/

BINDIR="./output/releaseFiles"
EXE="$BINDIR/folke.exe"
PREFIX="x86_64-w64-mingw32"
OBJDUMP_EXE="C:\ghcup\msys64\mingw64\bin\objdump.exe"

paths=("/usr/local/mingw64/bin"
    "/usr/local/mingw64/bin/x64"
    "/usr/$PREFIX/bin"
    "/usr/lib/gcc/$PREFIX/7.3-posix"
    "/usr/$PREFIX/lib"
    "C:\\ghcup\\msys64\\mingw64\\bin")

# neededDLLs="SDL2.dll
#     libfreetype-6.dll
#     glew32.dll
#     KERNEL32.dll
#     api-ms-win-crt-heap-l1-1-0.dll
#     api-ms-win-crt-private-l1-1-0.dll
#     api-ms-win-crt-runtime-l1-1-0.dll
#     api-ms-win-crt-stdio-l1-1-0.dll
#     api-ms-win-crt-string-l1-1-0.dll
#     SHELL32.dll
#     api-ms-win-crt-utility-l1-1-0.dll
#     api-ms-win-crt-math-l1-1-0.dll
#     api-ms-win-crt-environment-l1-1-0.dll
#     api-ms-win-crt-convert-l1-1-0.dll
#     api-ms-win-crt-locale-l1-1-0.dll
#     api-ms-win-crt-time-l1-1-0.dll
#     USER32.dll
#     ADVAPI32.dll
#     api-ms-win-crt-filesystem-l1-1-0.dll
#     WS2_32.dll
#     ole32.dll
#     RPCRT4.dll
#     ntdll.dll
#     dbghelp.dll
#     OPENGL32.dll
#     CRYPT32.dll
#     GDI32.dll
#     WINMM.dll
#     zlib1.dll"

checkedDLLs=()

function findAndCopyDLL() {
    if [[ $(echo ${checkedDLLs[@]} | fgrep -w $1) ]]
    then
        # echo "Already checked $1!!!!!!!!!!!!!!"
        return 0
    else
        checkedDLLs+=("$1")
    fi 

    for i in "${paths[@]}"
    do
        FILE="$i/$1"
        if [ -f $FILE ]; then
           cp $FILE $BINDIR
           echo "Found $1 in $i"
           copyForOBJ $FILE
           return 0
        fi
    done

    return 1
}

function copyForOBJ() {
    dlls=`$OBJDUMP_EXE -p $1 | grep 'DLL Name:' | sed -e "s/\t*DLL Name: //g"`
    while read -r filename; do
        findAndCopyDLL $filename || echo -e "\033[41mUnable to find $filename\033[0m"
    done <<< "$dlls"
}

echo "Calculating needed dlls (this will take some time)..."

copyForOBJ $EXE
# dlls=$neededDLLs
# echo "Locating dlls..."

# while read -r filename; do
#     findAndCopyDLL $filename || echo "Unable to find $filename"
# done <<< "$dlls"

echo -e "\033[46mDone!\033[0m"
# read -p "Press enter to continue"
