#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.22.4
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

XT_PATH="/Applications/XML/XT"
# XT_PATH="/usr/local/XT"

XSLT="lgtxhtml.xsl"

if [ -z "$1" ]; then
	title="Entity documentation index"
else
	title="$1"
fi

echo
echo This script converts all .xml files in the current directory to .html
echo files applying the XSLT transformation defined in the $XSLT file
echo using the James Clark XT XSLT Java processor 20020426a or later version.
echo
echo An index.html file, containing links to all .html documenting files,
echo is automatically generated. This file uses the script optional parameter 
echo value as the title of the index.html file.
echo
echo converting XML files to XHTML...

for file in *.xml; do
	echo "  converting" $file
	name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
	eval java -cp ${XT_PATH}/xt.jar:${XT_PATH}/lib/xp.jar -Dcom.jclark.xsl.sax.parser=com.jclark.xml.sax.CommentDriver com.jclark.xsl.sax.Driver $file $XSLT $name.html
done

echo conversion done
echo
echo generating index file...

echo "" > index.html

echo "<?xml version=\"1.0\"?>" >> index.html
echo "<?xml-stylesheet href=\"logtalk.css\" type=\"text/css\"?>" >> index.html
echo "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" >> index.html
echo "<html lang=\"en\" xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">" >> index.html
echo "<head>" >> index.html
echo "    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"/>" >> index.html
echo "    <title>"$title"</title>" >> index.html
echo "    <link rel=\"stylesheet\" href=\"logtalk.css\" type=\"text/css\">" >> index.html
echo "</head>" >> index.html
echo "<body>" >> index.html
echo "<h1>"$title"</h1>" >> index.html
echo "<ul>" >> index.html

for file in *.xml; do
	name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
	echo "  indexing" $name.html
	echo "    <li><a href=\""$name.html"\">"$name"</a></li>" >> index.html
done

echo "</ul>" >> index.html

date="`eval date`"

echo "<p>Generated on "$date"</p>" >> index.html
echo "</body>" >> index.html
echo "</html>" >> index.html

echo index file generated
echo
