#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.21.5
##
## Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
## =================================================================

XT_PATH="/Applications/XML/XT"
# XT_PATH="/usr/local/XT"

XSLT="lgthtml.xsl"

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
echo converting XML files to HTML...

for file in *.xml; do
	echo "  converting" $file
	name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
	eval java -cp ${XT_PATH}/xt.jar:${XT_PATH}/lib/xp.jar -Dcom.jclark.xsl.sax.parser=com.jclark.xml.sax.CommentDriver com.jclark.xsl.sax.Driver $file $XSLT $name.html
done

echo conversion done
echo
echo generating index file...

echo "" > index.html

echo "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" >> index.html
echo "<html>" >> index.html
echo "<head>" >> index.html
echo "    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">" >> index.html
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
