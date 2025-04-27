!include x64.nsh

!define PROG_NAME electra-stacking
!define PROG_NAME_REG ElectraStacking
!define PROG_NAME_USR "Electra Stacking"

ManifestSupportedOS Win7
SetCompressor /SOLID /FINAL lzma
SetCompressorDictSize 64
XPStyle on
Name "${PROG_NAME_USR}"
OutFile "${PROG_NAME}-setup-${PROG_VERS}-x64-win.exe"
RequestExecutionLevel admin
Unicode True
InstallDir $PROGRAMFILES64\${PROG_NAME_REG}
InstallDirRegKey HKLM "Software\${PROG_NAME_REG}" "Install_Dir"

Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

Function .onInit
    ${IfNot} ${RunningX64}
        MessageBox MB_OK "Sorry this application runs only on x64 machines"
        Abort
    ${EndIf}
FunctionEnd

Section "${PROG_NAME_USR} (required)"
    SectionIn RO
    SetOutPath $INSTDIR
    File /r "..\dist\ElectraStacking\*.*"
    WriteRegStr HKLM SOFTWARE\${PROG_NAME_REG} "Install_Dir" "$INSTDIR"
    WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PROG_NAME_REG}" "DisplayName" "${PROG_NAME_USR}"
    WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PROG_NAME_REG}" "UninstallString" '"$INSTDIR\uninstall.exe"'
    WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PROG_NAME_REG}" "NoModify" 1
    WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PROG_NAME_REG}" "NoRepair" 1
    WriteUninstaller "$INSTDIR\uninstall.exe"
SectionEnd

Section "Start Menu Shortcuts"
    CreateDirectory "$SMPROGRAMS\${PROG_NAME_REG}"
    CreateShortcut "$SMPROGRAMS\${PROG_NAME_REG}\Uninstall.lnk" "$INSTDIR\uninstall.exe"
    CreateShortcut "$SMPROGRAMS\${PROG_NAME_REG}\${PROG_NAME_USR}.lnk" "$INSTDIR\electra_stacking.exe"
SectionEnd

Section "Uninstall"
    DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PROG_NAME_REG}"
    DeleteRegKey HKLM SOFTWARE\${PROG_NAME_REG}
    Delete $INSTDIR\*
    RMDir /r /REBOOTOK $INSTDIR
    Delete "$SMPROGRAMS\${PROG_NAME_REG}\*.lnk"
    RMDir "$SMPROGRAMS\${PROG_NAME_REG}"
    RMDir "$INSTDIR"
SectionEnd
