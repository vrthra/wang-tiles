all: wang.pdf

cleanup:
	rm -f *.aux *.log *.out *.bbl *.blg

clobber: cleanup
	rm -f *.dvi *.pdf

%.pdf : %.tex %.bib
	latex $*
	bibtex $*
	latex $*
	pdflatex $*
	$(MAKE) cleanup

%.pdf : %.eps
	convert $*.eps $@
