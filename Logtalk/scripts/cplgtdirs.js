// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.29.3
//
// Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
// =================================================================

if (ScriptEngineMajorVersion() < 5 || ScriptEngineMajorVersion() == 5 && ScriptEngineMinorVersion() < 6) {
	WScript.Echo('Error! WSH 5.6 or later version needed for running this script.');
	WScript.Quit(1);
}

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
	WScript.Echo("The environment variable LOGTALKHOME must be defined first!");
	usage_help();
	WScript.Quit(1);
}

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (!FSObject.FolderExists(logtalk_home)) {
	WScript.Echo("The environment variable LOGTALKHOME points to a non-existing directory!");
	WScript.Echo("Its current value is: %LOGTALKHOME%");
	WScript.Echo("The variable must be set to your Logtalk installation directory!");
	WScript.Echo("");
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

if (FSObject.FolderExists(logtalk_user)) {
	var today = new Date();
	var year  = today.getFullYear();
	var month = today.getMonth() + 1;
	var day = today.getDate();
	if (day < 10)
        day = "0" + day;
	var hours = today.getHours();
	if (hours < 10)
        hours = "0" + hours;
	var mins = today.getMinutes();
	if (mins < 10)
        mins = "0" + mins;
	var secs = today.getSeconds();
	date = year + "-" + month + "-" + day + " " + hours + "-" + mins + "-" + secs;
	FSObject.MoveFolder(logtalk_user, logtalk_user + " backup " + date);
	WScript.Echo("Created a backup of the existing " + logtalk_user + " directory.");
	WScript.Echo("");
}

WScript.Echo("Creating LOGTALKUSER directory:");
WScript.Echo("");
WScript.Echo("  " + logtalk_user);
WScript.Echo("");

FSObject.CreateFolder(logtalk_user);

WScript.Echo("Copying Logtalk files and directories...");
FSObject.CopyFolder(logtalk_home + "\\configs", logtalk_user + "\\configs");
FSObject.CopyFile(logtalk_user + "\\configs\\xsb.config", logtalk_user + "\\configs\\xsb.pl");
FSObject.CopyFolder(logtalk_home + "\\contributions", logtalk_user + "\\contributions");
FSObject.CopyFolder(logtalk_home + "\\examples", logtalk_user + "\\examples");
FSObject.CopyFolder(logtalk_home + "\\libpaths", logtalk_user + "\\libpaths");
FSObject.CopyFile(logtalk_user + "\\libpaths\\libpaths.pl", logtalk_user + "\\libpaths\\libpaths_no_env_var.pl");
FSObject.CopyFolder(logtalk_home + "\\library", logtalk_user + "\\library");
FSObject.CopyFolder(logtalk_home + "\\xml", logtalk_user + "\\xml");

FSObject.DeleteFile(logtalk_user + "\\xml\\lgt2*.*");
FSObject.DeleteFile(logtalk_user + "\\xml\\logtalk.dtd");
FSObject.DeleteFile(logtalk_user + "\\xml\\logtalk.xsd");

var link = WshShell.CreateShortcut(logtalk_user + "\\manuals.lnk");
link.Description = "Shortcut to Logtalk documentation";
link.TargetPath = logtalk_home + "\\manuals";
link.Save();

link = WshShell.CreateShortcut(logtalk_user + "\\wenv.lnk");
link.Description = "Shortcut to Logtalk text editing support";
link.TargetPath = logtalk_home + "\\wenv";
link.Save();

link = WshShell.CreateShortcut(logtalk_user + "\\xml\\lgt2html.js.lnk");
link.Description = "Shortcut to lgt2html script";
link.TargetPath = logtalk_home + "\\xml\\lgt2html.js";
link.Save();

link = WshShell.CreateShortcut(logtalk_user + "\\xml\\lgt2pdf.js.lnk");
link.Description = "Shortcut to lgt2pdf script";
link.TargetPath = logtalk_home + "\\xml\\lgt2pdf.js";
link.Save();

link = WshShell.CreateShortcut(logtalk_user + "\\xml\\lgt2xml.js.lnk");
link.Description = "Shortcut to lgt2xml script";
link.TargetPath = logtalk_home + "\\xml\\lgt2xml.js";
link.Save();

link = WshShell.CreateShortcut(logtalk_user + "\\xml\\logtalk.dtd.lnk");
link.Description = "Shortcut to Logtalk DTD";
link.TargetPath = logtalk_home + "\\xml\\logtalk.dtd";
link.Save();

link = WshShell.CreateShortcut(logtalk_user + "\\xml\\logtalk.xsd.lnk");
link.Description = "Shortcut to Logtalk XML Schema";
link.TargetPath = logtalk_home + "\\xml\\logtalk.xsd";
link.Save();

WScript.Echo("Finished copying Logtalk files and directories.");
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
