// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.27.1
//
// Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
// =================================================================

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

WScript.Echo('');
WScript.Echo('Creating a shortcut named "Logtalk - CIAO" for running Logtalk');
WScript.Echo('with CIAO...');
WScript.Echo('');

var WshShell = new ActiveXObject("WScript.Shell");

var prolog_path = WshShell.RegRead("HKLM\\Software\\Ciao Prolog\\ciao_dir") + "\\shell\\ciaosh.cpx";

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (!FSObject.FileExists(prolog_path)) {
	WScript.Echo("Error! Cannot find ciaosh.cpx at the expected place!");
	WScript.Echo("Please, edit the script and update the location of the ciaosh.cpx executable.");
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

var f = FSObject.CreateTextFile(logtalk_home + "\\bin\\logtalk_ciao.pl", true);

f.WriteLine(":- ensure_loaded('\$LOGTALKUSER/configs/ciao_aux.config').");
f.WriteLine(":- set_prolog_flag(multi_arity_warnings, off).");
f.WriteLine(":- ensure_loaded('\$LOGTALKHOME/compiler/logtalk.pl').");
f.WriteLine(":- ensure_loaded('\$LOGTALKUSER/libpaths/libpaths.pl').");
f.WriteLine(":- op(600, xfy, ::).");
f.WriteLine(":- op(600, fy, ::).");
f.WriteLine(":- op(600, fy, ^^).");
f.WriteLine(":- op(200, fy, +).");
f.WriteLine(":- op(200, fy, ?).");
f.WriteLine(":- op(200, fy, @).");
f.WriteLine(":- op(200, fy, -).");
f.Close();

var ProgramsPath = WshShell.SpecialFolders("AllUsersPrograms");

if (!FSObject.FolderExists(ProgramsPath + "\\Logtalk")) 
	FSObject.CreateFolder(ProgramsPath + "\\Logtalk");

var link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk - CIAO.lnk");
link.Arguments = "-l %LOGTALKHOME%\\bin\\logtalk_ciao.pl";
link.Description = "Runs Logtalk with CIAO";
link.IconLocation = "app.exe,1";
link.TargetPath = prolog_path;
link.WindowStyle = 1;
link.WorkingDirectory = logtalk_home;
link.Save();

WScript.Echo('Done. The "Logtalk - CIAO" shortcut was been added to the');
WScript.Echo('Start Menu Programs. Make sure that the environment variables');
WScript.Echo('LOGTALKHOME and LOGTALKUSER are defined for all users wishing');
WScript.Echo('to use the shortcut.');
WScript.Echo('');
WScript.Echo('The first call to the shortcut must be made by a user with');
WScript.Echo('administrative rights.');
WScript.Echo('');
WScript.Echo('Users must run the batch script "cplgtdirs" before using the');
WScript.Echo('"Logtalk - CIAO" shortcut.');
WScript.Echo('');

WScript.Quit(0);

function usage_help() {
	WScript.Echo('');
	WScript.Echo('This script creates a shortcut named "Logtalk - CIAO" for running Logtalk');
	WScript.Echo('with CIAO. The script must be run by a user with administrative rights.');
	WScript.Echo('The LOGTALKHOME environment variable must be defined before running this');
	WScript.Echo('script.');
	WScript.Echo('');
	WScript.Echo('Usage:');
	WScript.Echo('  ' + WScript.ScriptName + ' help');
	WScript.Echo('  ' + WScript.ScriptName);
	WScript.Echo('');
}
