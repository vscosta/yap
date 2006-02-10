// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.27.0
//
// Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
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

if (WshUserEnv.Item("LOGTALKUSER")) {
	WScript.Echo("Reusing user environment variable LOGTALKUSER pointing to:");
	WScript.Echo("");
	WScript.Echo("  " + WshUserEnv.Item("LOGTALKUSER"));
}
else {
	WshUserEnv.Item("LOGTALKUSER") = WshShell.SpecialFolders("MyDocuments") + "\\logtalk";
	WScript.Echo("Defined user environment variable LOGTALKUSER pointing to:");
	WScript.Echo("");
	WScript.Echo("  " + WshShell.SpecialFolders("MyDocuments") + "\\logtalk");
}
WScript.Echo("");

WshSystemEnv.Item("PATH") = WshSystemEnv.Item("PATH") + ";%LOGTALKHOME%\\scripts;%LOGTALKHOME%\\xml";
WScript.Echo("Added Logtalk scripts and xml directories to the system PATH environment");
WScript.Echo("variable.");
WScript.Echo("");

FSObject.CopyFile(WshShell.CurrentDirectory + "\\BIBLIOGRAPHY", WshShell.CurrentDirectory + "\\BIBLIOGRAPHY.txt");
FSObject.CopyFile(WshShell.CurrentDirectory + "\\INSTALL", WshShell.CurrentDirectory + "\\INSTALL.txt");
FSObject.CopyFile(WshShell.CurrentDirectory + "\\LICENSE", WshShell.CurrentDirectory + "\\LICENSE.txt");
FSObject.CopyFile(WshShell.CurrentDirectory + "\\QUICK_START", WshShell.CurrentDirectory + "\\QUICK_START.txt");
FSObject.CopyFile(WshShell.CurrentDirectory + "\\README", WshShell.CurrentDirectory + "\\README.txt");
FSObject.CopyFile(WshShell.CurrentDirectory + "\\RELEASE_NOTES", WshShell.CurrentDirectory + "\\RELEASE_NOTES.txt");
FSObject.CopyFile(WshShell.CurrentDirectory + "\\UPGRADING", WshShell.CurrentDirectory + "\\UPGRADING.txt");

var ProgramsPath = WshShell.SpecialFolders("AllUsersPrograms");

if (!FSObject.FolderExists(ProgramsPath + "\\Logtalk")) 
	FSObject.CreateFolder(ProgramsPath + "\\Logtalk");

var link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk Web Site.url");

link.TargetPath = "http://www.logtalk.org/";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk HTML Documentation.lnk");

link.Description = "Browse Logtalk Documentation";
link.TargetPath = WshShell.CurrentDirectory + "\\manuals\\index.html";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk ReadMe.lnk");

link.Description = "Open Logtalk ReadMe";
link.TargetPath = WshShell.CurrentDirectory + "\\README.txt";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk Install and configuration.lnk");

link.Description = "Open Logtalk Install and configuration";
link.TargetPath = WshShell.CurrentDirectory + "\\INSTALL.txt";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk Quick Start.lnk");

link.Description = "Open Logtalk Quick Start";
link.TargetPath = WshShell.CurrentDirectory + "\\QUICK_START.txt";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk Bibliography.lnk");

link.Description = "Open Logtalk Bibliography";
link.TargetPath = WshShell.CurrentDirectory + "\\BIBLIOGRAPHY.txt";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk License.lnk");

link.Description = "Open Logtalk License";
link.TargetPath = WshShell.CurrentDirectory + "\\LICENSE.txt";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk Release Notes.lnk");

link.Description = "Open Logtalk Release Notes";
link.TargetPath = WshShell.CurrentDirectory + "\\RELEASE_NOTES.txt";
link.Save();

link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk Upgrading.lnk");

link.Description = "Open Logtalk Upgrading instructions";
link.TargetPath = WshShell.CurrentDirectory + "\\UPGRADING.txt";
link.Save();

WScript.Echo('Logtalk basic installation completed. You will need to RESTART in order');
WScript.Echo('to activate the new system environment variables and use the items in');
WScript.Echo('the new Logtalk program group. See the Install and configuration file');
WScript.Echo('for details on customizing your working environment.');
WScript.Echo('');
WScript.Echo('After restarting your computer, you may want to run some of the Prolog');
WScript.Echo('integration scripts, which you will find on the same directory as this');
WScript.Echo('installer script.');
WScript.Echo('');
WScript.Echo('Users must run the batch script cplgtdirs in order to copy the');
WScript.Echo('Logtalk user-modifiable files to their home directories. The path');
WScript.Echo('to the cplgtdirs, lgt2pdf, lgt2html, and lgt2xml batch scripts has');
WScript.Echo('been added to the system path environment variable.');
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
