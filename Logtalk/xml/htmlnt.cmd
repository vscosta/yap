@ECHO off

SET JAVA_HOME=c:\jdk1.3

SET XT_PATH=c:\xt
SET SAX_PATH=c:\xt
SET XP_PATH=c:\xt

SET XSLT=lgthtml.xsl
REM SET XSLT=lgtxhtml.xsl

IF "%1" == "" SET INDEX_TITLE=Entity documentation index
IF NOT "%1" == "" SET INDEX_TITLE=%1%

ECHO This script converts all .xml files in the current directory to .html
ECHO files applying the XSLT transformation defined in the $XSLT file
ECHO using the James Clark XT XSLT Java processor
ECHO.
ECHO An index.html file, containing links to all .html documenting files,
ECHO is automatically generated. This file uses the script optional parameter 
ECHO value as the title of the index.html file.
ECHO.
ECHO converting XML files to HTML...

FOR /f "tokens=1-2 delims=." %%f IN ('DIR /b *.xml') DO IF EXIST %%f.html DEL %%f.html

FOR /f "tokens=1-2 delims=." %%f IN ('DIR /b *.xml') DO %JAVA_HOME%\bin\java -cp "%XT_PATH%\xt.jar;%SAX_PATH%\sax.jar;%XP_PATH%\xp.jar" -Dcom.jclark.xsl.sax.parser=com.jclark.xml.sax.CommentDriver com.jclark.xsl.sax.Driver %%f.xml %XSLT% %%f.html

ECHO conversion done
ECHO.
ECHO generating index file...

IF EXIST index.html DEL index.html

ECHO ^<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd"^> >> index.html
ECHO ^<html^> >> index.html
ECHO ^<head^> >> index.html
ECHO ^<title^>%INDEX_TITLE%^</title^> >> index.html
ECHO ^<link rel="stylesheet" href="logtalk.css" type="text/css"^> >> index.html
ECHO ^</head^> >> index.html
ECHO ^<body^> >> index.html
ECHO ^<h1^>%INDEX_TITLE%^</h1^> >> index.html
ECHO ^<ul^> >> index.html

FOR /f "tokens=1-2 delims=." %%f IN ('DIR /b *.xml') DO ECHO ^<li^>^<a href="%%f.html"^>%%f^</a^>^</li^> >> index.html

ECHO ^</ul^> >> index.html

ECHO ^<p^>Generated on >> index.html
DATE/T >> index.html
TIME/T >> index.html
ECHO ^</p^> >> index.html

ECHO ^</body^> >> index.html
ECHO ^</html^> >> index.html

ECHO index file generated

@ECHO on
