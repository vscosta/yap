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

WScript.Echo('');
WScript.Echo('Creating shortcuts for running Logtalk with selected Prolog compilers...');
WScript.Echo('');

var WshShell = new ActiveXObject("WScript.Shell");
var WshSystemEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");

if (WshSystemEnv.Item("LOGTALKHOME"))
	logtalk_home = WshSystemEnv.Item("LOGTALKHOME");
else if (WshUserEnv.Item("LOGTALKHOME"))
	logtalk_home = WshUserEnv.Item("LOGTALKHOME")
else {
	WScript.Echo("Error! The environment variable LOGTALKHOME must be defined first!");
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

WScript.Echo('Creating shortcut Logtalk - B-Prolog...');
WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_bplgt.js", true);

WScript.Echo('Creating shortcut Logtalk - CIAO...');
WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_ciaolgt.js", true);
	
WScript.Echo('Creating shortcut Logtalk - ECLiPSe...');
WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_eclipselgt.js", true);

WScript.Echo('Creating shortcut Logtalk - GNU Prolog...');
WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_gplgt.js", true);

WScript.Echo('Creating shortcut Logtalk - K-Prolog...');
WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_plclgt.js", true);

WScript.Echo('Creating shortcut Logtalk - SICStus...');
WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_sicstuslgt.js", true);

WScript.Echo('Creating shortcut Logtalk - SWI-Prolog...');
WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_swilgt.js", true);

WScript.Echo('Creating shortcut Logtalk - XSB...');
WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_xsblgt.js", true);

WScript.Echo('Creating shortcut Logtalk - YAP...');
WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_yaplgt.js", true);

WScript.Echo('');
WScript.Echo('Done. Links to the created shortcuts are availasble from the Start Menu');
WScript.Echo('Logtalk program group. Ensure that the environment variables LOGTALKHOME');
WScript.Echo('and LOGTALKUSER are defined for all users wishing to use the shortcuts.');
WScript.Echo('');
WScript.Echo('Users must run the batch script "cplgtdirs" once prior to using the');
WScript.Echo('Logtalk - Prolog integration scripts.');
WScript.Echo('');
WScript.Echo('If you get an unexpected failure when creating or using a shortcut for');
WScript.Echo('one of the above Prolog compilers, make sure that the Prolog compiler');
WScript.Echo('is properly installed, consult the NOTES file on the scripts directory,');
WScript.Echo('and try to run the corresponding script individually.');
WScript.Echo('');

WScript.Quit(0);
