# Electra
Software for stacking of deep sky astronomical images

## Prerequisites

Windows + MSYS64:

```
pacman -S mingw-w64-x86_64-cfitsio
pacman -S mingw-w64-x86_64-gtk3 mingw-w64-x86_64-pkg-config
pacman -S base-devel mingw-w64-x86_64-gcc libxml2-devel tar
```
Linux:
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
