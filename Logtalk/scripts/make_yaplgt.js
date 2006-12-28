// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.29.1
//
// Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
// =================================================================

if (ScriptEngineMajorVersion() < 5 || ScriptEngineMajorVersion() == 5 && ScriptEngineMinorVersion() < 6) {
	WScript.Echo('Error! WSH 5.6 or later version needed for running this script.');
	WScript.Quit(1);
}

var prolog_path = "C:\\Yap\\bin\\yap.exe";

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

WScript.Echo('');
WScript.Echo('Creating a shortcut named "Logtalk - YAP" for running Logtalk');
WScript.Echo('with YAP...');
WScript.Echo('');

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (!FSObject.FileExists(prolog_path)) {
	WScript.Echo("Error! Cannot find yap.exe at the expected place!");
	WScript.Echo("Please edit the script and update the location of the yap.exe executable.");
	WScript.Quit(1);
}

var WshShell = new ActiveXObject("WScript.Shell");

var WshSystemEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");
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

if (!FSObject.FolderExists(logtalk_home)) {
	WScript.Echo("The environment variable LOGTALKHOME points to a non-existing directory!");
	WScript.Echo("Its current value is: %LOGTALKHOME%");
	WScript.Echo("The variable must be set to your Logtalk installation directory!");
	WScript.Echo("");
	usage_help();
	WScript.Quit(1);
}

logtalk_home = logtalk_home.replace(/\\/g, "\\\\");

if (!FSObject.FolderExists(logtalk_home + "\\bin")) 
	FSObject.CreateFolder(logtalk_home + "\\bin");

var f = FSObject.CreateTextFile(logtalk_home + "\\bin\\logtalk_yap.pl", true);

f.WriteLine(":- reconsult('$LOGTALKUSER\\\\configs\\\\yap.config').");
f.WriteLine(":- reconsult('$LOGTALKHOME\\\\compiler\\\\logtalk.pl').");
f.WriteLine(":- reconsult('$LOGTALKUSER\\\\libpaths\\\\libpaths.pl').");
f.Close();

var ProgramsPath = WshShell.SpecialFolders("AllUsersPrograms");

if (!FSObject.FolderExists(ProgramsPath + "\\Logtalk")) 
	FSObject.CreateFolder(ProgramsPath + "\\Logtalk");

var link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk - YAP.lnk");
link.Arguments = '-l "%LOGTALKHOME%\\bin\\logtalk_yap.pl"';
link.Description = 'Runs Logtalk with YAP';
link.IconLocation = 'app.exe,1';
link.TargetPath = prolog_path;
link.WindowStyle = 1;
link.WorkingDirectory = logtalk_home;
link.Save();

WScript.Echo('Done. The "Logtalk - YAP" shortcut was been added to the Start Menu');
WScript.Echo('Programs. Make sure that the environment variables LOGTALKHOME and');
WScript.Echo('LOGTALKUSER are defined for all users wishing to use the shortcut.');
WScript.Echo('');
WScript.Echo('Users must run the batch script "cplgtdirs" once before using the');
WScript.Echo('"Logtalk - YAP" shortcut.');
WScript.Echo('');

WScript.Quit(0);

function usage_help() {
	WScript.Echo('');
	WScript.Echo('This script creates a shortcut named "Logtalk - YAP" for running Logtalk');
	WScript.Echo('with YAP. The script must be run by a user with administrative rights.');
	WScript.Echo('The LOGTALKHOME environment variable must be defined before running this');
	WScript.Echo('script.');
	WScript.Echo('');
	WScript.Echo('Usage:');
	WScript.Echo('  ' + WScript.ScriptName + ' help');
	WScript.Echo('  ' + WScript.ScriptName);
	WScript.Echo('');
}
