
all: build clean

clean:
	rm -rf manual.toc manual.synctex.gz manual.aux manual.blg manual.log manual.log manual.out manual.bbl manual.dvi manual.ps texput.log

build:
	latex manual.tex
	bibtex manual.aux
	latex manual.tex
	latex manual.tex
	dvips -t a4 manual.dvi
	ps2pdf manual.ps

cleanall: clean
	rm -rf manual.pdf
