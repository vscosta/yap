// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.25.0
//
// Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
// =================================================================

var WshShell = new ActiveXObject("WScript.Shell");

var WshSystemEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");
var logtalk_home;
var logtalk_user;
	
if (WshSystemEnv.Item("LOGTALKHOME"))
	logtalk_home = WshSystemEnv.Item("LOGTALKHOME");
else if (WshUserEnv.Item("LOGTALKHOME"))
	logtalk_home = WshUserEnv.Item("LOGTALKHOME")
else {
	WScript.Echo("Error! The environment variable LOGTALKHOME must be defined first!");
	usage_help();
	WScript.Quit(1);
}

if (WScript.Arguments.Unnamed.Length > 0)
	usage_help();

if (WshUserEnv.Item("LOGTALKUSER"))
	logtalk_user = WshUserEnv.Item("LOGTALKUSER");
else {
	logtalk_user = WshShell.SpecialFolders("MyDocuments") + "\\logtalk";
	WshUserEnv.Item("LOGTALKUSER") = WshShell.SpecialFolders("MyDocuments") + "\\logtalk";
	WScript.Echo("Defined user environment variable LOGTALKUSER.");
	WScript.Echo("");
}

var fso = new ActiveXObject("Scripting.FileSystemObject");

if (fso.FolderExists(logtalk_user)) {
	WScript.Echo("Error! Logtalk directory already exists!");
	WScript.Echo("Please rename it and run this script again.");
	WScript.Echo("");
	usage_help();
}

WScript.Echo("Creating LOGTALKUSER directory:");
WScript.Echo("");
WScript.Echo("  " + logtalk_user);
WScript.Echo("");

fso.CreateFolder(logtalk_user);

WScript.Echo("Copying Logtalk files and directories...");
fso.CopyFolder(logtalk_home + "\\configs", logtalk_user + "\\configs");
fso.CopyFolder(logtalk_home + "\\contributions", logtalk_user + "\\contributions");
fso.CopyFolder(logtalk_home + "\\examples", logtalk_user + "\\examples");
fso.CopyFolder(logtalk_home + "\\libpaths", logtalk_user + "\\libpaths");
fso.CopyFolder(logtalk_home + "\\library", logtalk_user + "\\library");
fso.CopyFolder(logtalk_home + "\\xml", logtalk_user + "\\xml");

WScript.Echo("Finished copying Logtalk files directories.");
WScript.Echo("");
WScript.Echo("You may need to edit the contents of the file:");
WScript.Echo("");
WScript.Echo("  " + logtalk_user + "\\libpaths\\libpaths.pl");
WScript.Echo("");
WScript.Echo("to match your Prolog compiler and operating-system requirements or to");
WScript.Echo("add your own library paths.");
WScript.Echo("");
WScript.Echo("You may want to customize the default Logtalk compiler flags by editing");
WScript.Echo("the configuration file for your Prolog compiler found in the directory:");
WScript.Echo("");
WScript.Echo("  " + logtalk_user + "\\configs");
WScript.Echo("");

WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script copies the Logtalk user-modifiable files and directories");
	WScript.Echo("to the user home directory. The location can be set by the environment");
	WScript.Echo("variable \%LOGTALKUSER\% (defaults to MyDocuments\\logtalk when the");
	WScript.Echo("variable is not defined)");
	WScript.Echo("");
	WScript.Echo("Usage:");
	WScript.Echo("  " + WScript.ScriptName + " help");
	WScript.Echo("  " + WScript.ScriptName);
	WScript.Echo("");
	WScript.Quit(1);
}
