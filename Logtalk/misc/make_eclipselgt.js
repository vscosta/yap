// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.20.1
//
// Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
// =================================================================

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

WScript.Echo('');
WScript.Echo('Creating a shortcut named "Logtalk - ECLiPSe" for running Logtalk');
WScript.Echo('with ECLiPSe...');
WScript.Echo('');

var WshShell = new ActiveXObject("WScript.Shell");

var prolog_path = WshShell.RegRead("HKLM\\Software\\IC-Parc\\Eclipse\\5.7\\ECLIPSEDIR") + "\\lib\\i386_nt\\eclipse.exe";

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (!FSObject.FileExists(prolog_path)) {
	WScript.Echo("Error! Cannot find eclipse.exe at the expected place!");
	WScript.Quit(1);
}

var WshProcessEnv = WshShell.Environment("PROCESS");
var WshSystemEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");
var logtalk_home;

if (WshProcessEnv.Item("LOGTALKHOME"))
	logtalk_home = WshProcessEnv.Item("LOGTALKHOME");
else if (WshSystemEnv.Item("LOGTALKHOME"))
	logtalk_home = WshSystemEnv.Item("LOGTALKHOME");
else if (WshUserEnv.Item("LOGTALKHOME"))
	logtalk_home = WshUserEnv.Item("LOGTALKHOME")
else {
	WScript.Echo("Error! The system environment variable LOGTALKHOME must be defined first!");
	usage_help();
	WScript.Quit(1);
}

logtalk_home = logtalk_home.replace(/\\/g, "\\\\");

if (!FSObject.FolderExists(logtalk_home + "\\bin")) 
	FSObject.CreateFolder(logtalk_home + "\\bin");

var f = FSObject.CreateTextFile(logtalk_home + "\\bin\\lgtceclipse.pl", true);

f.WriteLine(":- pragma(system).");
f.WriteLine(":- pragma(nodebug).");
f.Close();

WshShell.Run("cmd /c type " + logtalk_home + "\\compiler\\logtalk.pl" + " >> " + logtalk_home + "\\bin\\lgtceclipse.pl", true);

f = FSObject.CreateTextFile(logtalk_home + "\\bin\\logtalkeclipse.pl", true);

f.WriteLine(":- ensure_loaded(library(toplevel)).");
f.WriteLine(":- cd('$LOGTALKHOME').");
f.WriteLine(":- compile('configs/eclipseiso.config').");
f.WriteLine(":- compile('bin/lgtceclipse.pl').");
f.Close();

var ProgramsPath = WshShell.SpecialFolders("AllUsersPrograms");
var link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk - ECLiPSe.lnk");
link.Arguments = "-b %LOGTALKHOME%\\bin\\logtalkeclipse.pl";
link.Description = "Runs Logtalk with ECLiPSe";
link.IconLocation = "app.exe,1";
link.TargetPath = prolog_path;
link.WindowStyle = 1;
link.WorkingDirectory = logtalk_home;
link.Save();

WScript.Echo('Done. The "Logtalk - ECLiPSe" shortcut was been added to the');
WScript.Echo('Start Menu Programs. Make sure that the LOGTALKHOME environment');
WScript.Echo('variable is defined for all users wishing to use the shortcut.');
WScript.Echo('');

WScript.Quit(0);

function usage_help() {
	WScript.Echo('');
	WScript.Echo('This script creates a shortcut named "Logtalk - ECLiPSe" for running Logtalk');
	WScript.Echo('with ECLiPSe. The script must be run by a user with administrative rights.');
	WScript.Echo('The LOGTALKHOME environment variable must be defined before running this');
	WScript.Echo('script.');
	WScript.Echo('');
	WScript.Echo('Usage:');
	WScript.Echo('  ' + WScript.ScriptName + ' help');
	WScript.Echo('  ' + WScript.ScriptName);
	WScript.Echo('');
}
