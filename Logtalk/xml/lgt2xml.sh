#!/bin/bash

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.25.1
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

xslt=lgtxml.xsl
spec=logtalk.dtd
css=logtalk.css
format=xhtml
index_file=index.html
index_title="Entity documentation index"

usage_help()
{
	echo 
	echo "This script generates an index for all the Logtalk XML files"
	echo "documenting files in the current directory"
	echo
	echo "Usage:"
	echo "  $0 -f format -i index -t title"
	echo "  $0 -h"
	echo
	echo "Optional arguments:"
	echo "  -f format of the index file (either xhtml or html; default is $format)"
	echo "  -i name of the index file (default is $index_file)"
	echo "  -t title to be used on the index file (default is $index_title)"
	echo "  -h help"
	echo
	exit 1
}

create_index_file()
{
	echo "" > "$index_file"

	case "$format" in
		xhtml)
			echo "<?xml version=\"1.0\" encoding=\"utf-8\"?>" >> "$index_file"
			echo "<?xml-stylesheet href=\"logtalk.css\" type=\"text/css\"?>" >> "$index_file"
			echo "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" >> "$index_file"
			echo "<html lang=\"en\" xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">" >> "$index_file"
			;;
		html)
			echo "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" >> "$index_file"
			echo "<html>" >> "$index_file"
			;;
	esac

	echo "<head>" >> "$index_file"
	echo "    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"/>" >> "$index_file"
	echo "    <title>"$index_title"</title>" >> "$index_file"
	echo "    <link rel=\"stylesheet\" href=\"logtalk.css\" type=\"text/css\"/>" >> "$index_file"
	echo "</head>" >> "$index_file"
	echo "<body>" >> "$index_file"
	echo "<h1>"$index_title"</h1>" >> "$index_file"
	echo "<ul>" >> "$index_file"

	for file in *.xml; do
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		entity=${name%_*}
		pars=${name##*_}
		echo "  indexing $file"
		if [ $pars -gt 0 ]
		then
			echo "    <li><a href=\""$file"\">"$entity"/"$pars"</a></li>" >> "$index_file"
		else
			echo "    <li><a href=\""$file"\">"$entity"</a></li>" >> "$index_file"
		fi
	done

	echo "</ul>" >> "$index_file"

	date="`eval date`"

	echo "<p>Generated on "$date"</p>" >> "$index_file"
	echo "</body>" >> "$index_file"
	echo "</html>" >> "$index_file"
}


if ! [ "$LOGTALKUSER" ]
then
	echo "Error! The environment variable LOGTALKUSER must be defined first!"
	exit 1
else

	while getopts "f:i:t:h" Option
	do
		case $Option in
			f) f_arg="$OPTARG";;
			i) i_arg="$OPTARG";;
			t) t_arg="$OPTARG";;
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

	if [[ "$i_arg" != "" ]]
	then
		index_file=$i_arg
	fi

	if [[ "$t_arg" != "" ]]
	then
		index_title=$t_arg
	fi

	cp "$LOGTALKUSER"/xml/$spec .
	cp "$LOGTALKUSER"/xml/$css .
	cp "$LOGTALKUSER"/xml/$xslt .

	echo
	echo "generating index file..."

	create_index_file

	echo "index file generated"
	echo

	exit 0

fi
