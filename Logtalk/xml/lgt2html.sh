#!/bin/bash

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.22.2
##
## Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
## =================================================================

html_xslt="$LOGTALKUSER/xml/lgthtml.xsl"
xhtml_xslt="$LOGTALKUSER/xml/lgtxhtml.xsl"

format=xhtml

directory="."

index_file=index.html
index_title="Entity documentation index"

processor=xsltproc
# processor=xalan
# processor=sabcmd

usage_help()
{
	echo 
	echo "This script converts all Logtalk XML files documenting files in the"
	echo "current directory to XHTML or HTML files"
	echo
	echo "Usage:"
	echo "  $0 -f format -d directory -i index -t title -p processor"
	echo "  $0 -h"
	echo
	echo "Optional arguments:"
	echo "  -f output file format (either xhtml or html; default is $format)"
	echo "  -d output directory for the generated files (default is $directory)"
	echo "  -i name of the index file (default is $index_file)"
	echo "  -t title to be used on the index file (default is $index_title)"
	echo "  -p XSLT processor (xsltproc, xalan, or sabcmd; default is $processor)"
	echo "  -h help"
	echo
	exit 1
}

create_index_file()
{
	echo "" > $index_file

	case "$format" in
		xhtml)
			echo "<?xml version=\"1.0\"?>" >> $index_file
			echo "<?xml-stylesheet href=\"logtalk.css\" type=\"text/css\"?>" >> $index_file
			echo "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" >> $index_file
			echo "<html lang=\"en\" xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">" >> $index_file
			;;
		html)
			echo "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" >> $index_file
			echo "<html>" >> $index_file
			;;
	esac

	echo "<head>" >> $index_file
	echo "    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"/>" >> $index_file
	echo "    <title>"$index_title"</title>" >> $index_file
	echo "    <link rel=\"stylesheet\" href=\"logtalk.css\" type=\"text/css\"/>" >> $index_file
	echo "</head>" >> $index_file
	echo "<body>" >> $index_file
	echo "<h1>"$index_title"</h1>" >> $index_file
	echo "<ul>" >> $index_file

	for file in *.xml; do
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		echo "  indexing $name.html"
		echo "    <li><a href=\""$name.html"\">"$name"</a></li>" >> $index_file
	done

	echo "</ul>" >> $index_file

	date="`eval date`"

	echo "<p>Generated on "$date"</p>" >> $index_file
	echo "</body>" >> $index_file
	echo "</html>" >> $index_file
}


if ! [ $LOGTALKUSER ]
then
	echo "Error! The environment variable LOGTALKUSER must be defined first!"
	exit 1
else

	while getopts "f:d:i:t:p:h" Option
	do
		case $Option in
			f) f_arg="$OPTARG";;
			d) d_arg="$OPTARG";;
			i) i_arg="$OPTARG";;
			t) t_arg="$OPTARG";;
			p) p_arg="$OPTARG";;
			h) usage_help;;
			*) usage_help;;
		esac
	done

	if [[ "$f_arg" != "" && "$f_arg" != "xhtml" && "$f_arg" != "html" ]]
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

	if [[ "$i_arg" != "" ]]
	then
		index_file=$i_arg
	fi

	if [[ "$t_arg" != "" ]]
	then
		index_title=$t_arg
	fi

	if [[ "$p_arg" != "" && "$p_arg" != "xsltproc" && "$p_arg" != "xalan" && "$p_arg" != "sabcmd" ]]
	then
		echo "Error! Unsupported XSLT processor: $p_arg"
		usage_help
		exit 1
	elif [ "$p_arg" != "" ]
	then
		processor=$p_arg
	fi

	if [ "$format" = "xhtml" ]
	then
		xslt=$xhtml_xslt
	else
		xslt=$html_xslt
	fi

	cp $LOGTALKUSER/xml/logtalk.dtd .
	cp $LOGTALKUSER/xml/logtalk.xsd .
	cp $LOGTALKUSER/xml/logtalk.css $directory

	echo
	echo "converting XML files..."

	for file in *.xml; do
		echo "  converting $file"
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		case "$processor" in
			xsltproc)	eval xsltproc -o $directory/$name.html $xslt $file;;
			xalan)		eval xalan -o $directory/$name.html $file $xslt;;
			sabcmd)		eval sabcmd $xslt $file $directory/$name.html;;
		esac
	done

	echo "conversion done"
	echo
	echo "generating index file..."

	index_file=$directory/$index_file
	create_index_file

	echo "index file generated"
	echo

	rm -f logtalk.dtd
	rm -f logtalk.xsd

	exit 0

fi
