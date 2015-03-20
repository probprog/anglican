all: paper.pdf paper.aux

PDFLATEX=pdflatex -interaction nonstopmode -file-line-error -shell-escape

%.aux: %.tex
	$(PDFLATEX) ${basename $<}

%.bbl: %.aux %.tex
	bibtex ${basename $<}

%.pdf: %.bbl %.tex
	$(PDFLATEX) ${basename $<}
	$(PDFLATEX) ${basename $<}

paper.pdf: paper.tex paper.aux refs.bib 

clean:
	rm -f *.out *.aux *.log *.bbl *.blg  *.fdb_latexmk *.fls *.pyg

distclean: clean
	rm -f paper.pdf
