@ECHO off

SET JAVA_HOME=c:\jdk1.3

SET XT_PATH=c:\xt

SET XSLT="lgthtml.xsl"
REM SET XSLT="lgtxhtml.xsl"

ECHO This script converts all .xml files in the current directory to .html
ECHO files applying the XSLT transformation defined in the $XSLT file
ECHO using the James Clark XT XSLT Java processor 20020426a or later version.

FOR %%f IN (*.xml) DO %JAVA_HOME%\bin\java -cp "%XT_PATH%\xt.jar;%XT_PATH%\lib\xp.jar" -Dcom.jclark.xsl.sax.parser=com.jclark.xml.sax.CommentDriver com.jclark.xsl.sax.Driver %%f %XSLT% %%f.html

REN *.xml.html *.html

ECHO conversion done

@ECHO on
