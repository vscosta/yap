// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.28.2
//
// Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
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
WScript.Echo('Creating a shortcut named "Logtalk - SICStus Prolog" for running Logtalk with');
WScript.Echo('SICStus Prolog 4.0 or 3.12 (edit the script if you are using other version)...');
WScript.Echo('');

var WshShell = new ActiveXObject("WScript.Shell");

var prolog_path;
var prolog_path4;
var prolog_path3;
var config_file;

try {
	prolog_path4 = WshShell.RegRead("HKLM\\Software\\SICS\\SICStus4.0_win32\\SP_PATH") + "\\bin\\spwin.exe";
}
catch(e) {
	prolog_path4 = "not_installed.lgt";
}
try {
	prolog_path3 = WshShell.RegRead("HKLM\\Software\\SICS\\SICStus3.12_win32\\SP_PATH") + "\\bin\\spwin.exe";
}
catch(e) {
	prolog_path3 = "not_installed.lgt";
}

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (FSObject.FileExists(prolog_path4)) {
	prolog_path = prolog_path4;
	config_file = "sicstus4.config";
}
else if (FSObject.FileExists(prolog_path3)) {
	prolog_path = prolog_path3;
	config_file = "sicstus.config";
}
else {
	WScript.Echo("Error! Cannot find spwin.exe at the expected places!");
	WScript.Echo("Please, edit the script and update the location of the spwin.exe executable.");
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

var f = FSObject.CreateTextFile(logtalk_home + "\\bin\\logtalk_sicstus.pl", true);

f.WriteLine(":- consult('$LOGTALKUSER/configs/" + config_file + "').");
f.WriteLine(":- consult('$LOGTALKHOME/compiler/logtalk.pl').");
f.WriteLine(":- consult('$LOGTALKUSER/libpaths/libpaths.pl').");
f.Close();

var ProgramsPath = WshShell.SpecialFolders("AllUsersPrograms");

if (!FSObject.FolderExists(ProgramsPath + "\\Logtalk")) 
	FSObject.CreateFolder(ProgramsPath + "\\Logtalk");

var link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk - SICStus Prolog.lnk");
link.Arguments = '-l "%LOGTALKHOME%\\bin\\logtalk_sicstus.pl"';
link.Description = 'Runs Logtalk with SICStus Prolog';
link.IconLocation = 'app.exe,1';
link.TargetPath = prolog_path;
link.WindowStyle = 1;
link.WorkingDirectory = logtalk_home;
link.Save();

WScript.Echo('Done. The "Logtalk - SICStus Prolog" shortcut was been added to');
WScript.Echo('the Start Menu Programs. Make sure that the environment variables');
WScript.Echo('LOGTALKHOME and LOGTALKUSER are defined for all users wishing');
WScript.Echo('to use the shortcut.');
WScript.Echo('');
WScript.Echo('Users must run the batch script "cplgtdirs" before using the');
WScript.Echo('"Logtalk - SICStus Prolog" shortcut.');
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
