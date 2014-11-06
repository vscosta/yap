; YAP install-script (based on a similar scrip from SWI-Prolog)

!define TEMP1 $R0 ; Temp variable
!define EXT    $3 ; Filename extension for Prolog sources
!define CWD    $4 ; Working directory for startmenu shortcut
!define GRP    $5 ; Startmenu group
!define SHCTX  $6 ; Shell context (current/all)
!define ARCH   $7 ; Architecture (x86, ia64 or amd64)

RequestExecutionLevel admin
MiscButtonText "<back" "next>" "abort" "finished"

SetCompressor /FINAL bzip2
  
; Preload files that are needed by the installer itself
ReserveFile "${NSISDIR}\Plugins\x86-unicode\UserInfo.dll"
ReserveFile "${NSISDIR}\Plugins\x86-unicode\InstallOptions.dll"
ReserveFile "${OPTIONS}"

!ifdef WIN64
InstallDir "$PROGRAMFILES64\${TARGET}"
!else
InstallDir "$PROGRAMFILES\${TARGET}"
!endif
InstallDirRegKey HKLM ${REGKEY} "home"

ComponentText "This will install YAP on your computer."
DirText "This program will install YAP on your computer.\
         Choose a directory"

Icon        ${ROOTDIR}\share\Yap\icons\yap.ico
LicenseData ${ROOTDIR}\share\doc\Yap\Artistic
LicenseText "YAP is governed by the Artistic License and LGPL;\
	it includes code under the GPL and LGPL."

InstType "Typical (all except debug symbols)"	# 1
InstType "Minimal (no graphics)"		# 2
InstType "Full"					# 3

Page license
Page directory
Page custom SetCustom "" ": Installation options"
Page instfiles

Section "Base system (required)"

  SectionIn RO			# do not allow to delete this
  
!ifdef WIN64
  SetRegView 64
!endif

  Delete $INSTDIR\bin\*.pdb

  SetOutPath $INSTDIR\bin
  File ${ROOTDIR}\bin\yap.exe
  File ${ROOTDIR}\bin\yap.dll
  File ${ROOTDIR}\bin\yap-win.exe
  File ${ROOTDIR}\bin\*.dll

; first, copy library DLLs
  SetOutPath $INSTDIR\lib\Yap
; SYSTEM STUFF
  File ${ROOTDIR}\lib\Yap\*.dll

  SetOutPath $INSTDIR\lib\Yap
; SYSTEM STUFF
  File ${ROOTDIR}\lib\Yap\startup.yss
  
  SetOutPath $INSTDIR\share\Yap
; SYSTEM STUFF
  File /r ${ROOTDIR}\share\Yap\*

  SetOutPath $INSTDIR\share\PrologCommons
; SYSTEM STUFF
  File /r ${ROOTDIR}\share\PrologCommons

  SetOutPath $INSTDIR\share\doc\Yap
  File /r ${ROOTDIR}\share\doc\Yap\html\*
;  File ${ROOTDIR}\share\doc\Yap\refman.pdf
;  File ${ROOTDIR}\share\doc\Yap\yap.info
  File ${ROOTDIR}\share\doc\Yap\Artistic
  File ${ROOTDIR}\share\doc\Yap\README.TXT
  File ${ROOTDIR}\share\doc\Yap\COPYING

  ; Write uninstaller
!ifdef WIN64
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\YAP64" "DisplayName" "YAP64 (remove only)"
!else
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\YAP" "DisplayName" "YAP (remove only)"
!endif
  WriteUninstaller "uninstall.exe"
SectionEnd


Section "Start Menu shortcuts"
  SectionIn 1 2 3
  SetOutPath ${CWD}
!ifdef WIN64
  StrCpy ${GRP} "YAP64"
!else
  StrCpy ${GRP} "YAP"
!endif
  CreateDirectory "$SMPROGRAMS\${GRP}"
  CreateShortCut "$SMPROGRAMS\${GRP}\YAP-WIN.lnk" \
		 "$INSTDIR\bin\yap-win.exe" \
		 "" \
		 "$INSTDIR\bin\yap-win.exe" \
		 0
  CreateShortCut "$SMPROGRAMS\${GRP}\YAP.lnk" \
		 "$INSTDIR\bin\yap.exe" \
		 "" \
		 "$INSTDIR\bin\yap.exe" \
		 0
  CreateShortCut "$SMPROGRAMS\${GRP}\Readme.lnk" \
  		  "$INSTDIR\share\doc\Yap\README.TXT" "" \
		  "$INSTDIR\share\doc\Yap\README.TXT" 0 \
		  "SW_SHOWNORMAL" "" "View readme"
  CreateShortCut "$SMPROGRAMS\${GRP}\Manual Html.lnk" \
  		  "$INSTDIR\share\doc\Yap\html\index.html" "" \
		  "$INSTDIR\share\doc\Yap\html\index.html" 0 \
		  "SW_SHOWNORMAL" "" "View readme"
;  CreateShortCut "$SMPROGRAMS\${GRP}\Manual PDF.lnk" \
;  		  "$INSTDIR\share\doc\Yap\refman.pdf" "" \
;		  "$INSTDIR\share\doc\Yap\refman.pdf" 0 \
;		  "SW_SHOWNORMAL" "" "View readme"
  CreateShortCut "$SMPROGRAMS\${GRP}\Uninstall.lnk" \
		 "$INSTDIR\uninstall.exe" \
		 "" \
		 "$INSTDIR\uninstall.exe" \
		 0

  WriteRegStr HKLM ${REGKEY} fileExtension   ${EXT}
  WriteRegStr HKLM ${REGKEY} group   ${GRP}
  WriteRegStr HKLM ${REGKEY} cwd     ${CWD}
  WriteRegStr HKLM ${REGKEY} context ${SHCTX}
SectionEnd

################################################################
# The uninstaller
################################################################

!ifdef WIN64
  UninstallText "This will uninstall YAP64. Hit Uninstall to continue."
!else
  UninstallText "This will uninstall YAP. Hit Uninstall to continue."
!endif

Section "Uninstall"
!ifdef WIN64
  SetRegView 64
!endif

  ReadRegStr ${EXT}   HKLM ${REGKEY} fileExtension
  StrCmp ${EXT} "" 0 UExt
    StrCpy ${EXT} "pl"
  UExt:

  ReadRegStr ${GRP}   HKLM ${REGKEY} group
  StrCmp ${GRP} "" 0 UHasGroup
!ifdef WIN64
    StrCpy ${GRP} "YAP64"
!else
    StrCpy ${GRP} "YAP"
!endif
  UHasGroup:

  ReadRegStr ${SHCTX} HKLM ${REGKEY} context
  StrCmp ${SHCTX} "" 0 UHasContext
    StrCpy ${SHCTX} "all"
  UHasContext:

  StrCmp ${SHCTX} "all" 0 +2
    SetShellVarContext all

  MessageBox MB_YESNO "Delete the following components?\r$\n \
                       Install dir: $INSTDIR$\r$\n \
		       Extension: ${EXT}$\r$\n \
		       Program Group ${GRP}" \
		      IDNO Done

  StrCmp "${EXT}" "" NoExt
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
!ifdef WIN64
    DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\YAP64"
!else
    DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\YAP"
!endif

    DeleteRegKey HKLM ${REGKEY}
SectionEnd

################################################################
# FUNCTIONS
################################################################

Function .onInit

  ;Extract InstallOptions files
  ;$PLUGINSDIR will automatically be removed when the installer closes
  
  InitPluginsDir
  File /oname=$PLUGINSDIR\options.ini "${OPTIONS}"

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
!ifdef WIN64
  SetRegView 64
!endif
# Basic system info
  Call UserInfo

# Filename extension
  ReadRegStr ${EXT} HKLM ${REGKEY} fileExtension
  StrCmp ${EXT} "" 0 HasExt
    StrCpy ${EXT} "pl"
  HasExt:
  WriteINIStr $PLUGINSDIR\options.ini "Field 4" "State" ${EXT}  

  StrCpy ${CWD} $INSTDIR

# Startmenu program group
  ReadRegStr ${GRP} HKLM ${REGKEY} group
  StrCmp ${GRP} "" 0 HasGroup
!ifdef WIN64
    StrCpy ${GRP} "YAP64"
!else
    StrCpy ${GRP} "YAP"
!endif
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
  ReadINIStr ${GRP} $PLUGINSDIR\options.ini "Field 5" "State"

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
  ExecShell "open" "$INSTDIR\share\doc\Yap\README.TXT"
  NoReadme:
FunctionEnd

Function .onInstFailed
  MessageBox MB_OK "Installation failed.$\r$\n\
		    If you cannot resolve the issue or it is a bug in the$\r$\n\
		    installer, please contact yap-users@sf.net"
FunctionEnd

outfile "${OUT_DIR}\yap${ABI}-${VERSION}-installer.exe"
