#!/bin/sh

FOP_PATH="/Applications/fop-0.20.4"

XSLT="lgtpdfa4.xsl"

echo 
echo This script converts all .xml files in the current directory to .pdf
echo files applying the XSLT transformation defined in the $XSLT file
echo using the Apache FOP processor
echo

for file in *.xml; do
	echo converting $file
	name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
	eval sh $FOP_PATH/fop.sh -q -xsl $XSLT -xml $file -pdf $name.pdf
done

echo
echo conversion done
echo
