// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.21.2
//
// Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
// =================================================================

var WshShell = new ActiveXObject("WScript.Shell");

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

if (WScript.Arguments.Unnamed.Length > 0)
	usage_help();

var user_home = WshShell.SpecialFolders("MyDocuments")

var logtalk_user = user_home + "\\logtalk";

var fso = new ActiveXObject("Scripting.FileSystemObject");

if (fso.FolderExists(logtalk_user)) {
	WScript.Echo("Error! Logtalk directory already exists! Please rename it or delete it.");
	WScript.Echo("");
	usage_help();
}

WScript.Echo("Creating directory " + logtalk_user + "...");
fso.CreateFolder(logtalk_user);

WScript.Echo("Copying Logtalk directories...");
fso.CopyFolder(logtalk_home + "\\examples", logtalk_user + "\\examples");
fso.CopyFolder(logtalk_home + "\\library", logtalk_user + "\\library");
fso.CopyFolder(logtalk_home + "\\xml", logtalk_user + "\\xml");

WScript.Echo("Finished copying Logtalk directories.");
WScript.Echo("");

WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script copies the Logtalk library, xml, and examples");
	WScript.Echo("directories to the user home directory (My Documents\\logtalk).");
	WScript.Echo("");
	WScript.Echo("Usage:");
	WScript.Echo("  " + WScript.ScriptName + " help");
	WScript.Echo("  " + WScript.ScriptName);
	WScript.Echo("");
	WScript.Quit(1);
}
