// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.22.0
//
// Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
// =================================================================

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

WScript.Echo('');
WScript.Echo('Completing Logtalk installation...');
WScript.Echo('');

var WshShell = new ActiveXObject("WScript.Shell");
var WshSystemEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");
var FSObject = new ActiveXObject("Scripting.FileSystemObject");

WshShell.CurrentDirectory = "..";

WshSystemEnv.Item("LOGTALKHOME") = WshShell.CurrentDirectory;
WScript.Echo("Defined system environment variable LOGTALKHOME pointing to:");
WScript.Echo("");
WScript.Echo("  " + WshShell.CurrentDirectory);
WScript.Echo("");

WshUserEnv.Item("LOGTALKUSER") = WshShell.SpecialFolders("MyDocuments") + "\\logtalk";
WScript.Echo("Defined user environment variable LOGTALKUSER pointing to:");
WScript.Echo("");
WScript.Echo("  " + WshShell.SpecialFolders("MyDocuments") + "\\logtalk");
WScript.Echo("");

FSObject.CopyFile(WshShell.CurrentDirectory + "\\BIBLIOGRAPHY", WshShell.CurrentDirectory + "\\BIBLIOGRAPHY.txt");
FSObject.CopyFile(WshShell.CurrentDirectory + "\\LICENSE", WshShell.CurrentDirectory + "\\LICENSE.txt");
FSObject.CopyFile(WshShell.CurrentDirectory + "\\QUICK_START", WshShell.CurrentDirectory + "\\QUICK_START.txt");
FSObject.CopyFile(WshShell.CurrentDirectory + "\\README", WshShell.CurrentDirectory + "\\README.txt");
FSObject.CopyFile(WshShell.CurrentDirectory + "\\RELEASE_NOTES", WshShell.CurrentDirectory + "\\RELEASE_NOTES.txt");
FSObject.CopyFile(WshShell.CurrentDirectory + "\\UPGRADING", WshShell.CurrentDirectory + "\\UPGRADING.txt");

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
link.TargetPath = "%LOGTALKHOME%\\README.txt";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk Quick Start.lnk");

link.Description = "Open Logtalk Quick Start";
link.TargetPath = "%LOGTALKHOME%\\QUICK_START.txt";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk Bibliography.lnk");

link.Description = "Open Logtalk Bibliography";
link.TargetPath = "%LOGTALKHOME%\\BIBLIOGRAPHY.txt";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk License.lnk");

link.Description = "Open Logtalk License";
link.TargetPath = "%LOGTALKHOME%\\LICENSE.txt";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk Release Notes.lnk");

link.Description = "Open Logtalk Release Notes";
link.TargetPath = "%LOGTALKHOME%\\RELEASE_NOTES.txt";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk Upgrading.lnk");

link.Description = "Open Logtalk Upgrading";
link.TargetPath = "%LOGTALKHOME%\\UPGRADING.txt";
link.Save();

WScript.Echo('Logtalk installation completed. You will need to restart in order');
WScript.Echo('to activate the new system environment variables and use the items');
WScript.Echo('in the new Logtalk program group.');
WScript.Echo('');

WScript.Quit(0);

function usage_help() {
	WScript.Echo('');
	WScript.Echo('This script completes the installation of Logtalk by setting the LOGTALKHOME');
	WScript.Echo('and LOGTALKUSER system environment variables and by creating a new program');
	WScript.Echo('group named "Logtalk" in the Windows Start Menu.');
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
