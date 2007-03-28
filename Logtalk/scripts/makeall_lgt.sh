#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.5
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
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

if ! [ -d "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME points to a non-existing directory!"
	echo "Its current value is: $LOGTALKHOME"
	echo "The variable must be set to your Logtalk installation directory!"
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

./make_bplgt.sh $prefix > /dev/null 2> /dev/null
if [ $? = 0 ] ; then
	echo "bplgt script created      (B-Prolog integration script)"
else
	echo "bplgt script creation failed      (B-Prolog integration script)"
fi

./make_ciaolgt.sh $prefix > /dev/null 2> /dev/null
if [ $? = 0 ] ; then
	echo "ciaolgt script created    (Ciao Prolog integration script)"
else
	echo "ciaolgt script creation failed    (Ciao Prolog integration script)"
fi

./make_eclipselgt.sh $prefix > /dev/null 2> /dev/null
if [ $? = 0 ] ; then
	echo "eclipselgt script created (ECLiPSe integration script)"
else
	echo "eclipselgt script creation failed (ECLiPSe integration script)"
fi

./make_gplgt.sh $prefix > /dev/null 2> /dev/null
if [ $? = 0 ] ; then
	echo "gplgt script created      (GNU Prolog integration script)"
else
	echo "gplgt script creation failed      (GNU Prolog integration script)"
fi

./make_plclgt.sh $prefix > /dev/null 2> /dev/null
if [ $? = 0 ] ; then
	echo "plclgt script created     (K-Prolog integration script)"
else
	echo "plclgt script creation failed     (K-Prolog integration script)"
fi

./make_qplgt.sh $prefix > /dev/null 2> /dev/null
if [ $? = 0 ] ; then
	echo "qplgt script created      (Qu-Prolog integration script)"
else
	echo "qplgt script creation failed      (Qu-Prolog integration script)"
fi

./make_sicstuslgt.sh $prefix > /dev/null 2> /dev/null
if [ $? = 0 ] ; then
	echo "sicstuslgt script created (SICStus Prolog integration script)"
else
	echo "sicstuslgt script creation failed (SICStus Prolog integration script)"
fi

./make_swilgt.sh $prefix > /dev/null 2> /dev/null
if [ $? = 0 ] ; then
	echo "swilgt script created     (SWI-Prolog integration script)"
else
	echo "swilgt script creation failed     (SWI-Prolog integration script)"
fi

./make_xsblgt.sh $prefix > /dev/null 2> /dev/null
if [ $? = 0 ] ; then
	echo "xsblgt script created     (XSB integration script)"
else
	echo "xsblgt script creation failed     (XSB integration script)"
fi

./make_yaplgt.sh $prefix > /dev/null 2> /dev/null
if [ $? = 0 ] ; then
	echo "yaplgt script created     (YAP integration script)"
else
	echo "yaplgt script creation failed     (YAP integration script)"
fi	

echo
echo "Done. Links to the created scripts can be found on $prefix/bin."
echo "Make sure that the Prolog compilers are also available on your "
echo "execution path."
echo
echo "If you got an unexpected failure when creating or using one of the"
echo "Prolog integration scripts, make sure that the Prolog compiler is"
echo "properly installed, consult the NOTES file on the scripts directory,"
echo "and try to run the corresponding script individually."
echo
echo "Users should ensure that the environment variables LOGTALKHOME"
echo "and LOGTALKUSER are defined and then run the \"cplgtdirs\" script"
echo "once prior to using the Prolog integration scripts."
echo
