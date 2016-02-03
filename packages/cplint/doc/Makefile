
manual.pdf: manual.tex bib.bib
	pdflatex manual
	bibtex manual
	pdflatex manual
	pdflatex manual

manual.html: manual.tex bib.bib
	pandoc manual.tex -o manual.html -s --bibliography bib.bib  --csl acm-sigchi-proceedings.csl --table-of-contents --toc-depth=2 
