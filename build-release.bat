echo off

:: Check all programs installed and accessible in PATH

call :check_exist cargo.exe
call :check_exist msgfmt.exe
call :check_exist 7za.exe
call :check_exist makensis.exe

:: getting MINGW_BIN path from `where msgfmt.exe`

FOR /F "tokens=* USEBACKQ" %%F IN (`where msgfmt.exe`) DO (
    SET MSGFMT_PATH=%%F
)
set MINGW_BIN=%MSGFMT_PATH%\..

:: Build

set RUSTFLAGS=
::cargo build --release
::if %errorlevel% neq 0 goto :error

:: Translate

md "%~dp0dist\ElectraStacking\locale\ru\LC_MESSAGES" 2>nul
msgfmt -o "%~dp0dist\ElectraStacking\locale\ru\LC_MESSAGES\electra_stacking.mo" "%~dp0po\ru.po"
if %errorlevel% neq 0 goto :error

:: Copy nesessary files and libraries

set GuiDist=%~dp0dist\ElectraStacking

rmdir /S /Q "%GuiDist%"
md "%GuiDist%"

xcopy /Y "%~dp0target\release\electra_stacking.exe" "%GuiDist%"
xcopy /Y /S "%~dp0target\debug\locale\ru\*.*" "%GuiDist%\locale\ru\*.*"
xcopy /Y "%MINGW_BIN%\gspawn-win64-helper.exe" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libgdk-3-*.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libglib-2.*-*.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libgdk_pixbuf-2.*-*.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libgio-2.*-*.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libgobject-2.*-*.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libgtk-3-*.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libgmodule-2.*-*.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libgobject-2.*-*.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libintl-8.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libffi-8.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libcairo-gobject-2.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libcairo-2.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libepoxy-0.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libfribidi-0.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libpango-1.*-*.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libpangocairo-1.*-*.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libpangowin32-1.*-*.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\zlib1.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libpcre2-8-0.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libiconv-2.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libgcc_s_seh-1.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libstdc++-6.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libfontconfig-1.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libfreetype-6.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libpixman-1-*.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libpng16-16.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libharfbuzz-0.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libpangoft2-1.*-*.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libharfbuzz-0.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libthai-0.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libwinpthread-1.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libexpat-1.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libwinpthread-1.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libbrotlidec.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libbz2-1.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libatk-1.*-*.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libdatrie-1.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libgraphite2.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libbrotlicommon.dll" "%GuiDist%"
xcopy /Y "%MINGW_BIN%\libcfitsio-3.dll" "%GuiDist%"


xcopy /Y /S "%MINGW_BIN%\..\share\glib-2.0\schemas\*.*" "%GuiDist%\share\glib-2.0\schemas\*.*"
xcopy /Y /S "%MINGW_BIN%\..\lib\gdk-pixbuf-2.0\*.*" "%GuiDist%\lib\gdk-pixbuf-2.0\*.*"
xcopy /Y /S "%MINGW_BIN%\..\share\icons\Adwaita\16x16\*.*" "%GuiDist%\share\icons\Adwaita\16x16\*.*"
xcopy /Y /S "%MINGW_BIN%\..\share\icons\hicolor\16x16\*.*" "%GuiDist%\share\icons\hicolor\16x16\*.*"
xcopy /Y /S "%MINGW_BIN%\..\share\icons\*.theme" "%GuiDist%\share\icons\*.*"

:: Find version

set vers="unknown"
for /f "eol=# tokens=1,2 delims== " %%i in (Cargo.toml) do (
    if "%%i"=="version" set vers=%%j
)
set vers=%vers:~1,-1%
echo VERSION=%vers%

:: Compress binaries

set ArchiveFile=dist\archives\electra-stacking-%vers%-x64-win.7z
md dist\archives
del "%ArchiveFile%"
7za.exe a -t7z -mx9 -r "%ArchiveFile%" "%GuiDist%/../*.*"
if %errorlevel% neq 0 goto :error

# Installer

makensis.exe /DPROG_VERS=%vers% "%~dp0win_stuff\installer.nsi"
if %errorlevel% neq 0 goto :error
move /Y "%~dp0win_stuff\*.exe" "%~dp0dist\archives"

exit /B

:check_exist
echo %1
set __path_for_file=
FOR /F "tokens=* USEBACKQ" %%F IN (`where %1`) DO (
    SET __path_for_file=%%F
)
if "%__path_for_file%"=="" (
    echo [93mFile %1 not exist
    echo Please install and put path to file in PATH variable [0m
    goto :error
)
exit /B

:error

echo [91mError :([0m
pause
exit