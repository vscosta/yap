// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.27.0
//
// Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
// =================================================================

var prolog_path = "C:\\Program Files\\XSB\\bin\\xsb.exe";

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

WScript.Echo('');
WScript.Echo('Creating a shortcut named "Logtalk - XSB CVS" for running Logtalk');
WScript.Echo('with XSB...');
WScript.Echo('');

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (!FSObject.FileExists(prolog_path)) {
	WScript.Echo("Error! Cannot find xsb.exe at the expected place!");
	WScript.Echo("Please edit the script and update the location of the xsb.exe executable.");
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

FSObject.CopyFile(logtalk_home + "\\compiler\\logtalk.pl", logtalk_home + "\\bin\\logtalkcvs.P");

var f = FSObject.CreateTextFile(logtalk_home + "\\bin\\logtalk_xsbcvs.P", true);

f.WriteLine(":- reconsult('$LOGTALKUSER\\\\configs\\\\xsbcvs.P').");
f.WriteLine(":- reconsult('$LOGTALKHOME\\\\bin\\\\logtalk.P').");
f.WriteLine(":- reconsult('$LOGTALKUSER\\\\libpaths\\\\libpaths.P').");
f.Close();

var ProgramsPath = WshShell.SpecialFolders("AllUsersPrograms");

if (!FSObject.FolderExists(ProgramsPath + "\\Logtalk")) 
	FSObject.CreateFolder(ProgramsPath + "\\Logtalk");

var link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk - XSB CVS.lnk");
link.Arguments = "-l %LOGTALKHOME%\\bin\\logtalk_xsbcvs.P";
link.Description = "Runs Logtalk with XSB CVS";
link.IconLocation = "app.exe,1";
link.TargetPath = prolog_path;
link.WindowStyle = 1;
link.WorkingDirectory = logtalk_home;
link.Save();

WScript.Echo('Done. The "Logtalk - XSB CVS" shortcut was been added to the Start Menu');
WScript.Echo('Programs.  Make sure that the environment variables LOGTALKHOME and');
WScript.Echo('LOGTALKUSER are defined for all users wishing to use the shortcut.');
WScript.Echo('');
WScript.Echo('The first call to the shortcut must be made by a user with');
WScript.Echo('administrative rights.');
WScript.Echo('');
WScript.Echo('Users must run the batch script "cplgtdirs" before using the');
WScript.Echo('"Logtalk - XSB CVS" shortcut. Users must edit the contents of');
WScript.Echo('the "libpaths.P" file on the "libpaths" directory in order to');
WScript.Echo('replace all occurrences of the LOGTALKUSER environment variable');
WScript.Echo('by its value.');
WScript.Echo('');

WScript.Quit(0);

function usage_help() {
	WScript.Echo('');
	WScript.Echo('This script creates a shortcut named "Logtalk - XSB CVS" for running Logtalk');
	WScript.Echo('with XSB. The script must be run by a user with administrative rights.');
	WScript.Echo('The LOGTALKHOME environment variable must be defined before running this');
	WScript.Echo('script.');
	WScript.Echo('');
	WScript.Echo('Usage:');
	WScript.Echo('  ' + WScript.ScriptName + ' help');
	WScript.Echo('  ' + WScript.ScriptName);
	WScript.Echo('');
}
