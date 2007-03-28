// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.29.5
//
// Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
// =================================================================

if (ScriptEngineMajorVersion() < 5 || ScriptEngineMajorVersion() == 5 && ScriptEngineMinorVersion() < 6) {
	WScript.Echo('Error! WSH 5.6 or later version needed for running this script.');
	WScript.Quit(1);
}

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

WScript.Echo('');
WScript.Echo('Creating a shortcut named "Logtalk - GNU Prolog" for running Logtalk');
WScript.Echo('with GNU Prolog...');
WScript.Echo('');

var WshShell = new ActiveXObject("WScript.Shell");

var prolog_path = WshShell.RegRead("HKCU\\Software\\GnuProlog\\RootPath") + "\\bin\\gprolog.exe";

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (!FSObject.FileExists(prolog_path)) {
	WScript.Echo("Error! Cannot find gprolog.exe at the expected place!");
	WScript.Echo("Please, edit the script and update the location of the gprolog.exe executable.");
	WScript.Quit(1);
}

var WshSystemEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");
var logtalk_home;

if (WshSystemEnv.Item("LOGTALKHOME"))
	logtalk_home = WshSystemEnv.Item("LOGTALKHOME");
else if (WshUserEnv.Item("LOGTALKHOME"))
	logtalk_home = WshUserEnv.Item("LOGTALKHOME")
else {
	WScript.Echo("Error! The system environment variable LOGTALKHOME must be defined first!");
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

FSObject.CopyFile(logtalk_home + "\\configs\\gnu.config", logtalk_home + "\\bin\\gnu.pl");

var f1 = FSObject.CreateTextFile(logtalk_home + "\\bin\\logtalk_gp.pl", true);
var f2 = FSObject.OpenTextFile(logtalk_home + "\\compiler\\logtalk.pl", 1);
var line;

f1.WriteLine(":- built_in.");
while (!f2.AtEndOfStream) {
	line = f2.ReadLine();
	f1.WriteLine(line);
}

f1.Close();
f2.Close();

var ProgramsPath = WshShell.SpecialFolders("AllUsersPrograms");

if (!FSObject.FolderExists(ProgramsPath + "\\Logtalk")) 
	FSObject.CreateFolder(ProgramsPath + "\\Logtalk");

var link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk - GNU Prolog.lnk");
link.Arguments = "--init-goal \"['$LOGTALKUSER/configs/gnu.config', '$LOGTALKHOME/bin/logtalk_gp.pl', '$LOGTALKUSER/libpaths/libpaths.pl']\"";
link.Description = "Runs Logtalk with GNU Prolog";
link.IconLocation = "app.exe,1";
link.TargetPath = prolog_path;
link.WindowStyle = 1;
link.WorkingDirectory = logtalk_home;
link.Save();

WScript.Echo('Done. The "Logtalk - GNU Prolog" shortcut was been added to the');
WScript.Echo('Start Menu Programs. Make sure that the environment variables');
WScript.Echo('LOGTALKHOME and LOGTALKUSER are defined for all users wishing');
WScript.Echo('to use the shortcut.');
WScript.Echo('');
WScript.Echo('Users must run the batch script "cplgtdirs" once before using');
WScript.Echo('the "Logtalk - GNU Prolog" shortcut.');
WScript.Echo('');

WScript.Quit(0);

function usage_help() {
	WScript.Echo('');
	WScript.Echo('This script creates a shortcut named "Logtalk - GNU Prolog" for running Logtalk');
	WScript.Echo('with GNU Prolog. The script must be run by a user with administrative rights.');
	WScript.Echo('The LOGTALKHOME environment variable must be defined before running this');
	WScript.Echo('script.');
	WScript.Echo('');
	WScript.Echo('Usage:');
	WScript.Echo('  ' + WScript.ScriptName + ' help');
	WScript.Echo('  ' + WScript.ScriptName);
	WScript.Echo('');
}
