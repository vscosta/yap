#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.21.5
##
## Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
## =================================================================

FOP_PATH="/Applications/XML/fop-0.20.5"
# FOP_PATH="/usr/local/fop-0.20.5"

XSLT="lgtpdfa4.xsl"
# XSLT="lgtpdfus.xsl"


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
