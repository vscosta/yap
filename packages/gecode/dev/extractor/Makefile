GECODEDIR := $(shell g++ $(CPPFLAGS) $(CXXFLAGS) -H -E gecodedir.hh 2>&1 >/dev/null | grep gecode/kernel.hh | awk '{print $$2}' | sed 's|/kernel.hh||')
GECODEDIR=/usr/local/opt/gecode
GECODECONFIG := $(GECODEDIR)/support/config.hpp
GECODEVERSION := $(shell cat $(GECODECONFIG) | egrep '\<GECODE_VERSION\>' | awk '{print $$3}' | sed 's/"//g')
GECODEVERSION="5.0.0"
PROTOTYPES = ../gecode-prototypes-$(GECODEVERSION).hh
ENUMS = ../gecode-enums-$(GECODEVERSION).py

all: $(PROTOTYPES) $(ENUMS)

$(PROTOTYPES): % : xml/namespaceGecode.xml extract-prototypes.xsl
	cat notice.hh > $@ || { rm -f $@; exit 1; }
	xsltproc extract-prototypes.xsl $<  \
	| grep EXPORT \
	| grep -v Symmetry \
	| sed 's/Gecode:://g' \
	| sed 's/< /</g' \
	| sed 's/ >/>/g' \
	| sed 's/ \&/\&/g' \
	| sed 's/, /,/g' \
	| sed 's/forceinline //g' \
	| sed 's/GECODE_INT_EXPORT //g' \
	| sed 's/GECODE_FLOAT_EXPORT //g' \
	| sed 's/GECODE_SET_EXPORT //g' \
	| grep -v '*' | sort >> $@ || { rm -f $@; exit 1; }

$(ENUMS): % : xml/namespaceGecode.xml extract-enums.xsl
	cat notice.py > $@ || { rm -f $@; exit 1; }
	xsltproc extract-enums.xsl $< >> $@ || { rm -f $@; exit 1; }

xml/namespaceGecode.xml: Doxyfile
	doxygen Doxyfile

Doxyfile: Doxyfile.in
	cat $< | sed "s#@GECODEDIR@#$(GECODEDIR)#" > $@ || { rm -f $@; exit 1; }

clean:
	-rm -rf *~ gecode-prototypes-*.hh gecode-enums-*.py Doxyfile xml
