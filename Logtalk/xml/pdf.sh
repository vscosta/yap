#!/bin/sh

FOP_PATH="/Applications/Fop-0.20.2"

XSLT="lgtpdfa4.xsl"

echo 
echo This script converts all .xml files in the current directory to .pdf
echo files applying the XSLT transformation defined in the $XSLT file
echo using the Apache FOP processor
echo

foreach file (*.xml)
	echo converting $file
	name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
	eval sh $FOP_PATH/fop.sh -xsl $XSLT -xml $file -pdf $name.pdf
end

echo
echo conversion done
echo
