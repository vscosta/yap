// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.21.0
//
// Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
// =================================================================

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

WScript.Echo('');
WScript.Echo('Creating a shortcut named "Logtalk - SICStus Prolog" for running Logtalk');
WScript.Echo('with SICStus Prolog...');
WScript.Echo('');

var WshShell = new ActiveXObject("WScript.Shell");

var prolog_path = WshShell.RegRead("HKLM\\Software\\SICS\\SICStus3.11_win32\\SP_PATH") + "\\bin\\spwin.exe";

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (!FSObject.FileExists(prolog_path)) {
	WScript.Echo("Error! Cannot find spwin.exe at the expected place!");
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
	WScript.Echo("Error! The environment variable LOGTALKHOME must be defined first!");
	usage_help();
	WScript.Quit(1);
}

logtalk_home = logtalk_home.replace(/\\/g, "\\\\");

if (!FSObject.FolderExists(logtalk_home + "\\bin")) 
	FSObject.CreateFolder(logtalk_home + "\\bin");

var f = FSObject.CreateTextFile(logtalk_home + "\\bin\\logtalksicstus.pl", true);

f.WriteLine(":- consult('$LOGTALKHOME\\\\configs\\\\sicstus.config').");
f.WriteLine(":- consult('$LOGTALKHOME\\\\compiler\\\\logtalk.pl').");
f.Close();

var ProgramsPath = WshShell.SpecialFolders("AllUsersPrograms");

if (!FSObject.FolderExists(ProgramsPath + "\\Logtalk")) 
	FSObject.CreateFolder(ProgramsPath + "\\Logtalk");

var link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk - SICStus Prolog.lnk");
link.Arguments = "-l %LOGTALKHOME%\\bin\\logtalksicstus.pl";
link.Description = "Runs Logtalk with SICStus Prolog";
link.IconLocation = "app.exe,1";
link.TargetPath = prolog_path;
link.WindowStyle = 1;
link.WorkingDirectory = logtalk_home;
link.Save();

WScript.Echo('Done. The "Logtalk - SICStus Prolog" shortcut was been added to the');
WScript.Echo('Start Menu Programs. Make sure that the LOGTALKHOME environment');
WScript.Echo('variable is defined for all users wishing to use the shortcut.');
WScript.Echo('');

WScript.Quit(0);

function usage_help() {
	WScript.Echo('');
	WScript.Echo('This script creates a shortcut named "Logtalk - SICStus Prolog" for');
	WScript.Echo('running Logtalk with SICStus Prolog. The script must be run by a user');
	WScript.Echo('with administrative rights. The LOGTALKHOME environment variable must');
	WScript.Echo('be defined before running this script.');
	WScript.Echo('');
	WScript.Echo('Usage:');
	WScript.Echo('  ' + WScript.ScriptName + ' help');
	WScript.Echo('  ' + WScript.ScriptName);
	WScript.Echo('');
}
