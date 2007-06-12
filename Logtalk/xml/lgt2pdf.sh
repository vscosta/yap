#!/bin/bash

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.30.1
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## ================================================================

if ! [ "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME should be defined first, pointing"
	echo "to your Logtalk installation directory!"
	echo "Trying the default locations for the Logtalk installation..."
	if [ -d "/usr/local/share/logtalk" ]; then
		LOGTALKHOME=/usr/local/share/logtalk
		echo "... using Logtalk installation found at /usr/local/share/logtalk"
	elif [ -d "/usr/share/logtalk" ]; then
		LOGTALKHOME=/usr/share/logtalk
		echo "... using Logtalk installation found at /usr/share/logtalk"
	elif [ -d "/opt/local/share/logtalk" ]; then
		LOGTALKHOME=/opt/local/share/logtalk
		echo "... using Logtalk installation found at /opt/local/share/logtalk"
	elif [ -d "/opt/share/logtalk" ]; then
		LOGTALKHOME=/opt/share/logtalk
		echo "... using Logtalk installation found at /opt/share/logtalk"
	else
		echo "... unable to locate Logtalk installation directory!"
		echo
		exit 1
	fi
	echo
elif ! [ -d "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME points to a non-existing directory!"
	echo "Its current value is: $LOGTALKHOME"
	echo "The variable must be set to your Logtalk installation directory!"
	echo
	exit 1
fi
export LOGTALKHOME

if ! [ "$LOGTALKUSER" ]; then
	echo "The environment variable LOGTALKUSER should be defined first, pointing"
	echo "to your Logtalk user directory!"
	echo "Trying the default location for the Logtalk user directory..."
	export LOGTALKUSER=$HOME/logtalk
	if [ -d "$LOGTALKUSER" ]; then		
		echo "... using Logtalk user directory found at $LOGTALKUSER"
	else
		echo "... Logtalk user directory not found at default location. Creating a"
		echo "new Logtalk user directory by running the \"cplgtdirs\" shell script:"
		cplgtdirs
	fi
elif ! [ -d "$LOGTALKUSER" ]; then
	echo "Cannot find \$LOGTALKUSER directory! Creating a new Logtalk user directory"
	echo "by running the \"cplgtdirs\" shell script:"
	cplgtdirs
fi
echo

a4_xsl="$LOGTALKUSER/xml/lgtpdfa4.xsl"
us_xsl="$LOGTALKUSER/xml/lgtpdfus.xsl"

format=a4
# format=us

processor=fop
# processor=xep
# processor=xinc

directory="."

usage_help()
{
	echo 
	echo "This script converts all Logtalk XML documenting files in the"
	echo "current directory to PDF files"
	echo
	echo "Usage:"
	echo "  $0 -f format -d directory -p processor"
	echo "  $0 -h"
	echo
	echo "Optional arguments:"
	echo "  -f paper format (either a4 or us; default is $format)"
	echo "  -d output directory for the PDF files (default is $directory)"
	echo "  -p XSL-FO processor (either fop, xep, or xinc; default is $processor)"
	echo "  -h help"
	echo
	exit 1
}

while getopts "f:d:p:h" Option
do
	case $Option in
		f) f_arg="$OPTARG";;
		d) d_arg="$OPTARG";;
		p) p_arg="$OPTARG";;
		h) usage_help;;
		*) usage_help;;
	esac
done

if [[ "$f_arg" != "" && "$f_arg" != "a4" && "$f_arg" != "us" ]]
then
	echo "Error! Unsupported output format: $f_arg"
	usage_help
	exit 1
elif [ "$f_arg" != "" ]
then
	format=$f_arg
fi

if [[ "$d_arg" != "" && ! -d "$d_arg" ]]
then
	echo "Error! directory does not exists: $d_arg"
	usage_help
	exit 1
elif [ "$d_arg" != "" ]
then
	directory=$d_arg
fi

if [[ "$p_arg" != "" && "$p_arg" != "fop" && "$p_arg" != "xep" && "$p_arg" != "xinc" ]]
then
	echo "Error! Unsupported XSL-FO processor: $p_arg"
	usage_help
	exit 1
elif [ "$p_arg" != "" ]
then
	processor=$p_arg
fi

if [ "$format" = "a4" ]
then
	xsl=$a4_xsl
else
	xsl=$us_xsl
fi

if ! [[ -a "./logtalk.dtd" ]]
then
	cp "$LOGTALKHOME"/xml/logtalk.dtd .
fi

if ! [[ -a "./custom.ent" ]]
then
	cp "$LOGTALKUSER"/xml/custom.ent .
fi

if ! [[ -a "./logtalk.xsd" ]]
then
	cp "$LOGTALKHOME"/xml/logtalk.xsd .
fi

if [[ `(ls *.xml | wc -l) 2> /dev/null` -gt 0 ]]
then
	echo
	echo "converting XML files to PDF..."
	for file in *.xml; do
		echo "  converting $file"
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		case $processor in
			xinc)	eval xinc -xml \"$file\" -xsl \"$xsl\" -pdf \"$directory\"/\"$name.pdf\" 2> /dev/null;;
			*)		eval $processor -q -xml \"$file\" -xsl \"$xsl\" -pdf \"$directory\"/\"$name.pdf\";;
		esac
	done
	echo "conversion done"
	echo
else
	echo
	echo "No XML files exist in the current directory!"
	echo
fi

rm -f logtalk.dtd
rm -f logtalk.xsd

exit 0
