// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.21.4
//
// Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
// =================================================================

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

WScript.Echo('');
WScript.Echo('Installing Logtalk...');
WScript.Echo('');

var WshShell = new ActiveXObject("WScript.Shell");
var WshSystemEnv = WshShell.Environment("SYSTEM");
var FSObject = new ActiveXObject("Scripting.FileSystemObject");

WshShell.CurrentDirectory = "..";
WshSystemEnv.Item("LOGTALKHOME") = WshShell.CurrentDirectory;

if (!FSObject.FolderExists(WshShell.CurrentDirectory + "\\bin"))
	FSObject.CreateFolder(WshShell.CurrentDirectory + "\\bin");

FSObject.CopyFile(WshShell.CurrentDirectory + "\\README", WshShell.CurrentDirectory + "\\bin\\README.txt");
FSObject.CopyFile(WshShell.CurrentDirectory + "\\QUICK_START", WshShell.CurrentDirectory + "\\bin\\QUICK_START.txt");

var ProgramsPath = WshShell.SpecialFolders("AllUsersPrograms");

if (!FSObject.FolderExists(ProgramsPath + "\\Logtalk")) 
	FSObject.CreateFolder(ProgramsPath + "\\Logtalk");

var link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk Web Site.lnk");

link.Description = "Go to Logtalk Web Site";
link.TargetPath = "http://www.logtalk.org/";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk HTML Documentation.lnk");

link.Description = "Browse Logtalk Documentation";
link.TargetPath = "%LOGTALKHOME%\\manuals\\index.html";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk ReadMe.lnk");

link.Description = "Open Logtalk ReadMe";
link.TargetPath = "%LOGTALKHOME%\\bin\\README.txt";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk Quick Start.lnk");

link.Description = "Open Logtalk Quick Start";
link.TargetPath = "%LOGTALKHOME%\\bin\\QUICK_START.txt";
link.Save();

WScript.Echo('Logtalk installation completed.');
WScript.Echo('');

WScript.Quit(0);

function usage_help() {
	WScript.Echo('');
	WScript.Echo('This script completes the installation of Logtalk by setting the LOGTALKHOME');
	WScript.Echo('system environment variable and by creating a new program group named "Logtalk"');
	WScript.Echo('in the Windows Start Menu.');
	WScript.Echo('');
	WScript.Echo('The script must be run from this directory, by a user with administration');
	WScript.Echo('privileges, after decompressing the Logtalk distribution into its final');
	WScript.Echo('destination.');
	WScript.Echo('');
	WScript.Echo('Usage:');
	WScript.Echo('  ' + WScript.ScriptName + ' help');
	WScript.Echo('  ' + WScript.ScriptName);
	WScript.Echo('');
}
