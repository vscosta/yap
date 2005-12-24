// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.26.2
//
// Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
// =================================================================

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

WScript.Echo('Creating shortcut Logtalk - XSB CVS...');
WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_xsbcvslgt.js", true);

WScript.Echo('Creating shortcut Logtalk - YAP...');
WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_yaplgt.js", true);

WScript.Echo('');
WScript.Echo('Done.  Links to the created scripts was been added to the Start Menu');
WScript.Echo('Programs.   Make sure that the environment variables LOGTALKHOME and');
WScript.Echo('LOGTALKUSER are defined for all users wishing to use the shortcuts.');
WScript.Echo('');
WScript.Echo('Users must run the batch script "cplgtdirs" before using the Logtalk -');
WScript.Echo('Prolog integration scripts.');
WScript.Echo('');
WScript.Echo('If you get an unexpected failure to create a shortcut for one of the');
WScript.Echo('above Prolog compilers, make sure that the Prolog compiler is properly');
WScript.Echo('installed, consult the NOTES file on the scripts directory, and try to');
WScript.Echo('run the corresponding script individually.');
WScript.Echo('');

WScript.Quit(0);
