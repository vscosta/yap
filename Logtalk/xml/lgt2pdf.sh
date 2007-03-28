#!/bin/bash

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.5
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## =================================================================

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

if ! [ "$LOGTALKHOME" ]
then
	echo "Error! The environment variable LOGTALKHOME must be defined first!"
	exit 1
elif ! [ "$LOGTALKUSER" ]
then
	echo "Error! The environment variable LOGTALKUSER must be defined first!"
	exit 1
else

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

fi
