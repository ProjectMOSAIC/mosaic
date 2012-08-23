PDFS = chapter01.pdf chapter02.pdf chapter03.pdf chapter04.pdf chapter05.pdf chapter06.pdf chapter07.pdf chapter08.pdf chapter09.pdf chapter10.pdf chapter11.pdf chapter13.pdf

all: $(PDFS)

clean:
	rm -f *.log *.aux *.vrb *.toc
	

%.pdf : %.Rnw
	./knitr $< 
	pdflatex $*
	pdflatex $*

