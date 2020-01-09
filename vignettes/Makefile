default:
	make vectools.pdf
	make remove_other

vectools.pdf: vectools.tex
	pdflatex vectools.tex

remove_other:
	rm -f *.tex
	rm -f *.aux
	rm -f *.log
	rm -f temp*
