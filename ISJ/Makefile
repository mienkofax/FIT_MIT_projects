NAME=doc

all:
	latex $(NAME) .tex
	latex $(NAME) .tex   
	dvips -t a4 $(NAME).dvi
	ps2pdf $(NAME).ps
clean:
	-rm -f $(NAME).aux $(NAME).ps $(NAME).dvi $(NAME).log

cleanall:	
	-rm -f $(NAME).aux $(NAME).ps $(NAME).dvi $(NAME).log $(NAME).pdf $(NAME).out

