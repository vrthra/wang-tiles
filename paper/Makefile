all: wang.pdf

clean:
	rm *.aux *.log *.out *.pdf *.bbl *.blg *.dvi

%.pdf : %.tex %.bib
	latex $*
	bibtex $*
	latex $*
	pdflatex $*

%.pdf : %.eps
	convert $*.eps $@
