FILE=iclp07

all:	$(FILE).dvi $(FILE).ps $(FILE).pdf

$(FILE).dvi: force
	./latexmk $(FILE)

$(FILE).ps: $(FILE).dvi
	dvips -t letterSize -P cmz -o $(FILE).ps $(FILE).dvi

$(FILE).pdf: $(FILE).ps
	ps2pdf14 $(FILE).ps

.PHONY: force
force:

.PHONY: clean
clean::
	rm -f $(FILE).aux $(FILE).aux.bak $(FILE).bbl $(FILE).blg $(FILE).dvi $(FILE).log

.PHONY: realclean
realclean: clean
	rm -f $(FILE).ps $(FILE).pdf
