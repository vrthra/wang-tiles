all: wang.pdf

cleanup:
	rm -f *.aux *.log *.out *.bbl *.blg

clobber: cleanup
	rm -f *.dvi *.pdf

%.pdf : %.tex %.bib
	latex $*
	bibtex $*
	latex $*
	pdflatex --shell-escape --synctex=1 $*
	$(MAKE) cleanup

#epstool --copy --bbox wang13.eps 1.eps

%.pdf : %.eps
	convert $*.eps $@
