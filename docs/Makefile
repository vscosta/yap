#
# generate documentation files for YAP
#

MAKEINFO=makeinfo
TEXI2DVI=texi2dvi
TEXI2HTML=texi2html
TEXI2PDF=texi2pdf

all: info html ps pdf

install-info:
	$(MAKEINFO) $(MAKEINFOFLAGS) -I $(srcdir) -o $(INFODIR)/yap.info $(srcdir)/Documentation/yap.tex

info: yap.info

yap.info: yap.tex 
	$(MAKEINFO) yap.tex

html: yap.html

yap.html: yap.tex
	$(TEXI2HTML) yap.tex

dvi: yap.dvi

yap.dvi: yap.tex
	$(TEXI2DVI) yap.tex

ps:	yap.ps

yap.ps: yap.dvi
	dvips -o yap.ps yap

pdf: yap.pdf

yap.pdf: yap.tex
	$(TEXI2PDF) yap.tex

clean:
	rm -f *.aux *.ca *.cas *.cn *.cns *.cp *.cps *.cy *.cys *.dvi *.fn *.fns *.ky *.log *.sa *.sas *.sn *.sns *.sy *.sys *~ *.tp *.vr *.toc *.pg

realclean:
	-@make clean
	rm -f yap.ps yap.html yap_toc.html yap.pdf yap.info*

