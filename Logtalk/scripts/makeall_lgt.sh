#! /bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.25.1
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating scripts for running Logtalk with selected Prolog compilers..."
echo

if ! [ "$LOGTALKHOME" ]
then
	echo "The environment variable LOGTALKHOME must be defined first!"
	echo
	exit 1
fi

if [ -z "$1" ]; then
	prefix=/usr/local
else
	prefix="$1"
fi

if ! [ -d "$prefix" ]; then
	echo "Directory prefix does not exist!"
	echo
	exit 1
fi

./make_ciaolgt.sh $prefix > /dev/null
if [ $? ]; then
	echo "ciaolgt script created"
else
	echo "ciaolgt script creation failed"
fi

./make_eclipselgt.sh $prefix > /dev/null
if [ $? ]; then
	echo "eclipselgt script created"
else
	echo "eclipselgt script creation failed"
fi

./make_gplgt.sh $prefix > /dev/null
if [ $? ]; then
	echo "gplgt script created"
else
	echo "gplgt script creation failed"
fi

./make_plclgt.sh $prefix > /dev/null
if [ $? ]; then
	echo "plclgt script created"
else
	echo "plclgt script creation failed"
fi

./make_sicstuslgt.sh $prefix > /dev/null
if [ $? ]; then
	echo "sicstuslgt script created"
else
	echo "sicstuslgt script creation failed"
fi

./make_swilgt.sh $prefix > /dev/null
if [ $? ]; then
	echo "swilgt script created"
else
	echo "swilgt script creation failed"
fi

./make_xsblgt.sh $prefix > /dev/null
if [ $? ]; then
	echo "xsblgt script created"
else
	echo "xsblgt script creation failed"
fi

./make_yaplgt.sh $prefix > /dev/null
if [ $? ]; then
	echo "yaplgt script created"
else
	echo "yaplgt script creation failed"
fi	

echo
echo "Done. Links to the created scripts can be found on $prefix/bin."
echo "Make sure that the Prolog compilers are also available on your "
echo "execution path.  Users should define the environment variables "
echo "LOGTALKHOME and LOGTALKUSER in order to use the scripts."
echo
echo "If you get an unexpected failure to create a shortcut for one of the"
echo "above Prolog compilers, please consult the NOTES file on the scripts"
echo "directory or try to run the corresponding script individually."
echo
