// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.26.2
//
// Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
// =================================================================

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

WScript.Echo('');
WScript.Echo('Creating a shortcut named "Logtalk - ECLiPSe" for running Logtalk with');
WScript.Echo('ECLiPSe 5.8 (edit this script if you are using a different version)...');
WScript.Echo('');

var WshShell = new ActiveXObject("WScript.Shell");

var prolog_path = WshShell.RegRead("HKLM\\Software\\IC-Parc\\Eclipse\\5.8\\ECLIPSEDIR") + "\\lib\\i386_nt\\eclipse.exe";

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (!FSObject.FileExists(prolog_path)) {
	WScript.Echo("Error! Cannot find eclipse.exe at the expected place!");
	WScript.Echo("Please, edit the script and update the location of the eclipse.exe executable.");
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

var f = FSObject.CreateTextFile(logtalk_home + "\\bin\\lgtc_eclipse.pl", true);

f.WriteLine(":- pragma(system).");
f.WriteLine(":- pragma(nodebug).");
f.Close();

WshShell.Run("cmd /c type " + logtalk_home + "\\compiler\\logtalk.pl" + " >> " + logtalk_home + "\\bin\\lgtc_eclipse.pl", true);

f = FSObject.CreateTextFile(logtalk_home + "\\bin\\logtalk_eclipse.pl", true);

f.WriteLine(":- ensure_loaded(library(toplevel)).");
f.WriteLine(":- cd('$LOGTALKUSER').");
f.WriteLine(":- compile('configs/eclipseiso.config').");
f.WriteLine(":- cd('$LOGTALKHOME').");
f.WriteLine(":- compile('bin/lgtc_eclipse.pl').");
f.WriteLine(":- cd('$LOGTALKUSER').");
f.WriteLine(":- compile('libpaths/libpaths.pl').");
f.Close();

var ProgramsPath = WshShell.SpecialFolders("AllUsersPrograms");

if (!FSObject.FolderExists(ProgramsPath + "\\Logtalk")) 
	FSObject.CreateFolder(ProgramsPath + "\\Logtalk");

var link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk - ECLiPSe.lnk");
link.Arguments = "-b %LOGTALKHOME%\\bin\\logtalk_eclipse.pl";
link.Description = "Runs Logtalk with ECLiPSe";
link.IconLocation = "app.exe,1";
link.TargetPath = prolog_path;
link.WindowStyle = 1;
link.WorkingDirectory = logtalk_home;
link.Save();

WScript.Echo('Done. The "Logtalk - ECLiPSe" shortcut was been added to the');
WScript.Echo('Start Menu Programs. Make sure that the environment variables');
WScript.Echo('LOGTALKHOME and LOGTALKUSER are defined for all users wishing');
WScript.Echo('to use the shortcut.');
WScript.Echo('');
WScript.Echo('Users must run the batch script "cplgtdirs" before using the');
WScript.Echo('"Logtalk - ECLiPSe" shortcut.');
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
