all: wang.pdf

cleanup:
	-rm *.aux *.log *.out *.pdf *.bbl *.blg *.dvi

%.pdf : %.tex %.bib
	latex $*
	bibtex $*
	latex $*
	pdflatex $*
	make $(cleanup)

%.pdf : %.eps
	convert $*.eps $@
