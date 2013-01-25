# YAP install-script (from SWI-Prolog)

!define TEMP1 $R0 ; Temp variable
!define EXT    $3 ; Filename extension for Prolog sources
!define CWD    $4 ; Working directory for startmenu shortcut
!define GRP    $5 ; Startmenu group
!define SHCTX  $6 ; Shell context (current/all)
!define ARCH   $7 ; Architecture (x86, ia64 or amd64)

!ifdef WIN64
!define REGKEY SOFTWARE\YAP\Prolog64
!else
!define REGKEY SOFTWARE\YAP\Prolog
!endif


RequestExecutionLevel admin
SetCompressor bzip2
MiscButtonText "<back" "next>" "abort" "finished"

# Preload files that are needed by the installer itself
ReserveFile "${NSISDIR}\Plugins\UserInfo.dll"
ReserveFile "${NSISDIR}\Plugins\InstallOptions.dll"
ReserveFile "options.ini"

InstallDir $PROGRAMFILES\Yap
InstallDirRegKey HKLM ${REGKEY} "home"
ComponentText "This will install YAP on your computer."
DirText "This program will install YAP on your computer.\
         Choose a directory"

LicenseData c:\Yap\share\doc\Yap\Artistic
LicenseText "YAP is governed by the Artistic License,\
	but includes code under the GPL and LGPL."

InstType "Typical (all except debug symbols)"	# 1
InstType "Minimal (no graphics)"		# 2
InstType "Full"					# 3

Page license
Page directory
Page custom SetCustom "" ": Installation options"
Page instfiles

Section "Base system (required)"
  SectionIn RO			# do not allow to delete this

  Delete $INSTDIR\bin\*.pdb

  SetOutPath $INSTDIR\bin
  File c:\Yap\bin\yap.exe
  File c:\Yap\bin\yap.dll
  File c:\Yap\bin\yap-win.exe
  File c:\Yap\bin\plterm.dll

  SetOutPath $INSTDIR\bin
; SYSTEM STUFF
  File c:\Yap\lib\Yap\*.dll

  SetOutPath $INSTDIR\lib

  SetOutPath $INSTDIR\lib
; SYSTEM STUFF
  File c:\Yap\lib\Yap\startup.yss

  SetOutPath $INSTDIR\share
; SYSTEM STUFF
  File /r c:\Yap\share\Yap\*

  SetOutPath $INSTDIR\doc\Yap
  File c:\Yap\share\doc\Yap\yap.html
  File c:\Yap\share\doc\Yap\yap.pdf
  File c:\Yap\share\doc\Yap\Artistic
  File c:\Yap\share\doc\Yap\README.TXT
  File c:\Yap\share\doc\Yap\COPYING.TXT

  WriteRegStr HKLM ${REGKEY} "home" "$INSTDIR"
  WriteRegStr HKLM ${REGKEY} "startup" "$INSTDIR\lib\startup.yss"
  WriteRegStr HKLM ${REGKEY} "library" "$INSTDIR\share"

  ; Write uninstaller
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\YAP" "DisplayName" "YAP (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\YAP" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteUninstaller "uninstall.exe"
SectionEnd


Section "Start Menu shortcuts"
  SectionIn 1 2 3
  SetOutPath ${CWD}
  CreateDirectory "$SMPROGRAMS\${GRP}"
  CreateShortCut "$SMPROGRAMS\${GRP}\YAP-WIN.lnk" \
		 "$INSTDIR\bin\yap-win.exe" \
		 "" \
		 "$INSTDIR\bin\yap-win.exe" \
		 0
  SetOutPath $INSTDIR
  CreateShortCut "$SMPROGRAMS\${GRP}\Readme.lnk" \
  		  "$INSTDIR\doc\Yap\README.TXT" "" \
		  "$INSTDIR\doc\Yap\README.TXT" 0 \
		  "SW_SHOWNORMAL" "" "View readme"
  CreateShortCut "$SMPROGRAMS\${GRP}\Manual Html.lnk" \
  		  "$INSTDIR\doc\Yap\yap.html" "" \
		  "$INSTDIR\doc\Yap\yap.html" 0 \
		  "SW_SHOWNORMAL" "" "View readme"
  CreateShortCut "$SMPROGRAMS\${GRP}\Manual PDF.lnk" \
  		  "$INSTDIR\doc\Yap\yap.pdf" "" \
		  "$INSTDIR\doc\Yap\yap.pdf" 0 \
		  "SW_SHOWNORMAL" "" "View readme"
  CreateShortCut "$SMPROGRAMS\${GRP}\Uninstall.lnk" \
		 "$INSTDIR\uninstall.exe" \
		 "" \
		 "$INSTDIR\uninstall.exe" \
		 0

  WriteRegStr HKLM ${REGKEY} group   ${GRP}
  WriteRegStr HKLM ${REGKEY} cwd     ${CWD}
  WriteRegStr HKLM ${REGKEY} context ${SHCTX}
SectionEnd

################################################################
# The uninstaller
################################################################

UninstallText "This will uninstall YAP. Hit Uninstall to continue."

Section "Uninstall"
  ReadRegStr ${EXT}   HKLM Software\YAP\Prolog fileExtension
  ReadRegStr ${GRP}   HKLM Software\YAP\Prolog group
  ReadRegStr ${SHCTX} HKLM Software\YAP\Prolog context

  StrCmp ${SHCTX} "all" 0 +2
    SetShellVarContext all

  MessageBox MB_YESNO "Delete the following components?$\r$\n \
                       Install dir: $INSTDIR$\r$\n \
		       Extension: ${EXT}$\r$\n \
		       Program Group ${GRP}" \
		      IDNO Done

  StrCmp ".${EXT}" "" NoExt
    ReadRegStr $1 HKCR .${EXT} ""
    StrCmp $1 "PrologFile" 0 NoOwn ; only do this if we own it
      ReadRegStr $1 HKCR .${EXT} "backup_val"
      StrCmp $1 "" 0 RestoreBackup ; if backup == "" then delete the whole key
	DeleteRegKey HKCR .${EXT}
      Goto NoOwn
      RestoreBackup:
	WriteRegStr HKCR .${EXT} "" $1
	DeleteRegValue HKCR .${EXT} "backup_val"
    NoOwn:
  NoExt:

  StrCmp "${GRP}" "" NoGrp
    MessageBox MB_OK "Deleting $SMPROGRAMS\${GRP}"
    RMDir /r "$SMPROGRAMS\${GRP}"
  NoGrp:

  IfFileExists "$INSTDIR\bin\yap.exe" 0 NoDir
    RMDir /r "$INSTDIR"
    goto Done

  NoDir:
    MessageBox MB_OK "Folder $INSTDIR doesn't seem to contain Prolog"

  Done:
    DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\YAP"
    DeleteRegKey HKLM ${REGKEY}
SectionEnd

################################################################
# FUNCTIONS
################################################################

Function .onInit

  ;Extract InstallOptions files
  ;$PLUGINSDIR will automatically be removed when the installer closes
  
  InitPluginsDir
  File /oname=$PLUGINSDIR\options.ini "options.ini"

FunctionEnd

################################################################
# Handle customisation;  Settings are maintained in
#
# 	HKLM ${REGKEY}
#
# Using the following mapping:
#
#	${EXT} fileExtension
################################################################

Function SetCustom
# Basic system info
  Call UserInfo

# Filename extension
  ReadRegStr ${EXT} HKLM ${REGKEY} fileExtension
  StrCmp ${EXT} "" 0 HasExt
    StrCpy ${EXT} "pl"
  HasExt:
  WriteINIStr $PLUGINSDIR\options.ini "Field 4" "State" ${EXT}  

# Startmenu program group
  ReadRegStr ${GRP} HKLM ${REGKEY} group
  StrCmp ${GRP} "" 0 HasGroup
    StrCpy ${GRP} "YAP"
  HasGroup:
  WriteINIStr $PLUGINSDIR\options.ini "Field 6" "State" ${GRP}  

# Start the dialog
  Push ${TEMP1}
  InstallOptions::dialog "$PLUGINSDIR\options.ini"
  Pop ${TEMP1}
  Pop ${TEMP1}

# Get the results
  ReadINIStr ${EXT} $PLUGINSDIR\options.ini "Field 4" "State"
  ReadINIStr ${GRP} $PLUGINSDIR\options.ini "Field 6" "State"
FunctionEnd

Function UserInfo
  ClearErrors
  UserInfo::GetName
  IfErrors Win9x
  Pop $0
  UserInfo::GetAccountType
  Pop $1

  StrCmp $1 "Admin" 0 +4
    SetShellVarContext all
    StrCpy ${SHCTX} "all"
    Goto done
  StrCmp $1 "Power" 0 +3
    StrCpy ${SHCTX} "all"
    Goto done
  StrCmp $1 "User" 0 +3
    StrCpy ${SHCTX} "current"
    Goto done
  StrCmp $1 "Guest" 0 +3
    StrCpy ${SHCTX} "current"
    Goto done
  StrCpy ${SHCTX} "current"		# Unkown accounttype
    Goto done

  Win9x:
    StrCpy ${SHCTX}  "current"
    Goto end

  done:
    StrCmp ${SHCTX} "all" 0 +2
      SetShellVarContext all

  end:
FunctionEnd

Function .onInstSuccess
  MessageBox MB_YESNO "Installation complete. View readme?" IDNO NoReadme
  ExecShell "open" "$INSTDIR\doc\README.TXT"
  NoReadme:
FunctionEnd

Function .onInstFailed
  MessageBox MB_OK "Installation failed.$\r$\n\
		    If you cannot resolve the issue or it is a bug in the$\r$\n\
		    installer, please contact yap-users@sf.net"
FunctionEnd

outfile "yap-6.3.4-installer.exe"
