// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.25.1
//
// Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
// =================================================================

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

WScript.Echo('');
WScript.Echo('Creating a shortcut named "Logtalk - K-Prolog" for running Logtalk');
WScript.Echo('with K-Prolog...');
WScript.Echo('');

var WshShell = new ActiveXObject("WScript.Shell");
var WshSystemEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");

var prolog_path;

if (WshSystemEnv.Item("PLC"))
	prolog_path = WshSystemEnv.Item("PLC") + "\\plc.exe";
else if (WshUserEnv.Item("PLC"))
	prolog_path = WshUserEnv.Item("PLC") + "\\plc.exe"
else {
	WScript.Echo("Error! The K-Prolog environment variable PLC must be defined first!");
	usage_help();
	WScript.Quit(1);
}

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (!FSObject.FileExists(prolog_path)) {
	WScript.Echo("Error! Cannot find plc.exe at the expected place!");
	WScript.Quit(1);
}

var logtalk_home;

if (WshSystemEnv.Item("LOGTALKHOME"))
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

f = FSObject.CreateTextFile(logtalk_home + "\\bin\\logtalk_plc.rc", true);

f.WriteLine(":- consult('$LOGTALKUSER/configs/k.config').");
f.WriteLine(":- consult('$LOGTALKHOME/compiler/logtalk.pl').");
f.WriteLine(":- consult('$LOGTALKUSER/libpaths/libpaths.pl').");
f.Close();

var ProgramsPath = WshShell.SpecialFolders("AllUsersPrograms");

if (!FSObject.FolderExists(ProgramsPath + "\\Logtalk")) 
	FSObject.CreateFolder(ProgramsPath + "\\Logtalk");

var link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk - K-Prolog.lnk");
link.Arguments = "-h 2048k -l 1024k -g 2048k -e \"(consult('$LOGTALKHOME/bin/logtalk_plc.rc'), '$root').\"";
link.Description = "Runs Logtalk with K-Prolog";
link.IconLocation = "app.exe,1";
link.TargetPath = prolog_path;
link.WindowStyle = 1;
link.WorkingDirectory = logtalk_home;
link.Save();

WScript.Echo('Done. The "Logtalk - K-Prolog" shortcut was been added to the');
WScript.Echo('Start Menu Programs. Make sure that the environment variables');
WScript.Echo('LOGTALKHOME and LOGTALKUSER are defined for all users wishing');
WScript.Echo('to use the shortcut.');
WScript.Echo('');

WScript.Quit(0);

function usage_help() {
	WScript.Echo('');
	WScript.Echo('This script creates a shortcut named "Logtalk - K-Prolog" for');
	WScript.Echo('running Logtalk with K-Prolog. The script must be run by a user');
	WScript.Echo('with administrative rights. The LOGTALKHOME environment variable');
	WScript.Echo('must be defined before running this script.');
	WScript.Echo('');
	WScript.Echo('Usage:');
	WScript.Echo('  ' + WScript.ScriptName + ' help');
	WScript.Echo('  ' + WScript.ScriptName);
	WScript.Echo('');
}
