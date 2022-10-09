# Electra
Software for stacking of deep sky astronomical images

## Prerequisites for Windows
* Rust compiler (i686-pc-windows-**gnu**): https://www.rust-lang.org/tools/install
* MSYS: https://www.msys2.org/
* Libs and tools (type inside MSYS command line):
```
pacman -S mingw-w64-x86_64-cfitsio
pacman -S mingw-w64-x86_64-gtk3 mingw-w64-x86_64-pkg-config
pacman -S base-devel mingw-w64-x86_64-gcc libxml2-devel tar
```
Env. variable for `gettext-rs` (can be set globally in Windows setting or locally before build):
```
set MSYSTEM_PREFIX=c:\msys64\mingw64
```

## Prerequisites for Linux
* Rust compiler: https://www.rust-lang.org/tools/install
* Libs and tools:
```
sudo apt install gcc libgtk-3-dev build-essential libcfitsio-dev
```

## How to build
To build optimized binaries just type
```
cargo build --release
```
## Simple documentation
Unfortunately only google translation https://art--den-github-io.translate.goog/electra_stacking/?_x_tr_sl=ru&_x_tr_tl=en

## Why Electra?
Electra is star in the Pleiades cluster: https://en.wikipedia.org/wiki/Electra_(star)
