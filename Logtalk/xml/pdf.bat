@ECHO off

SET FOP_PATH=c:\Fop-0.20.2

SET XSLT="lgtpdfa4.xsl"

ECHO This script converts all .xml files in the current directory to .pdf
ECHO files applying the XSLT transformation defined in the $XSLT file
ECHO using the Apache FOP processor

FOR %%f IN (*.xml) DO %FOP_PATH%\fop.bat -xsl %XSLT% -xml %%f -pdf %%f.pdf

REN *.xml.pdf *.pdf

ECHO conversion done

@ECHO on
