# in order to produce pdf or html from manual.tex you have 
# to manually edit manual.tex following the instructions at the beginning of
# it

manual.pdf: manual.tex manual.bbl
	pdflatex manual
	pdflatex manual

manual.html: manual.tex manual.bbl
	htlatex manual

