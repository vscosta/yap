#!/bin/bash

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.30.1
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## ================================================================

css2xslfo=/Applications/XML/CSSToXSLFO/css2xslfo1_4_2.jar

xslt_proc=xsltproc
# xslt_proc=xalan
# xslt_proc=sabcmd

#fo_proc=fop
fo_proc=xep
# fo_proc=xinc

rm -f userman.fo userman.html
rm -fr *.section
eval $xslt_proc -o index.section userman.xsl index.html
eval $xslt_proc -o features.section userman.xsl features.html
eval $xslt_proc -o nomenclature.section userman.xsl nomenclature.html
eval $xslt_proc -o messages.section userman.xsl messages.html
eval $xslt_proc -o objects.section userman.xsl objects.html
eval $xslt_proc -o protocols.section userman.xsl protocols.html
eval $xslt_proc -o categories.section userman.xsl categories.html
eval $xslt_proc -o predicates.section userman.xsl predicates.html
eval $xslt_proc -o inheritance.section userman.xsl inheritance.html
eval $xslt_proc -o events.section userman.xsl events.html
eval $xslt_proc -o threads.section userman.xsl threads.html
eval $xslt_proc -o errors.section userman.xsl errors.html
eval $xslt_proc -o documenting.section userman.xsl documenting.html
eval $xslt_proc -o installing.section userman.xsl installing.html
eval $xslt_proc -o running.section userman.xsl running.html
eval $xslt_proc -o programming.section userman.xsl programming.html

cat -s \
	userman.header \
	index.section \
	userman.body \
	features.section nomenclature.section messages.section \
	objects.section protocols.section categories.section predicates.section \
	inheritance.section events.section threads.section errors.section \
	documenting.section installing.section running.section programming.section \
	userman.footer \
	> userman.html

java -jar $css2xslfo userman.html -fo userman.fo
eval $fo_proc -fo userman.fo -pdf userman.pdf
rm userman.fo userman.html
rm -fr *.section
