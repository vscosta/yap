GECODEDIR := $(shell g++ -q $(CPPFLAGS) $(CXXFLAGS) -H -E gecodedir.hh 2>&1 >/dev/null | grep gecode/kernel.hh | awk '{print $$2}' | sed 's|/kernel.hh||')
GECODECONFIG := $(GECODEDIR)/support/config.hpp
GECODEVERSION := $(shell cat $(GECODECONFIG) | egrep GECODE_VERSION | awk '{print $$3}' | sed 's/"//g')
PROTOTYPES = gecode-prototypes-$(GECODEVERSION).hh
ENUMS = gecode-enums-$(GECODEVERSION).py

all: $(PROTOTYPES) $(ENUMS)

$(PROTOTYPES): % : xml/namespaceGecode.xml extract-prototypes.xsl
	cat notice.hh > $@
	xsltproc extract-prototypes.xsl $<  \
	| sed 's/Gecode:://g' \
	| sed 's/< /</g' \
	| sed 's/ >/>/g' \
	| sed 's/ \&/\&/g' \
	| sed 's/, /,/g' \
	| sed 's/forceinline //g' \
	| sed 's/GECODE_INT_EXPORT //g' \
	| sed 's/GECODE_SET_EXPORT //g' \
	| grep -v '*' | sort >> $@

$(ENUMS): % : xml/namespaceGecode.xml extract-enums.xsl
	cat notice.py > $@
	xsltproc extract-enums.xsl $< >> $@

xml/namespaceGecode.xml: Doxyfile
	doxygen Doxyfile

Doxyfile: Doxyfile.in
	cat $< | sed "s#@GECODEDIR@#$(GECODEDIR)#" > $@

clean:
	-rm -rf *~ gecode-prototypes-*.hh gecode-enums-*.py Doxyfile xml
