.PHONY: all install clean distclean

all: whitepaper.pdf

whitepaper.pdf: whitepaper.tex
	pdflatex -interaction=nonstopmode whitepaper.tex
	pdflatex -interaction=nonstopmode whitepaper.tex

install:
	chmod +x install_tex_deps.sh
	./install_tex_deps.sh

clean:
	rm -f *.aux *.log *.out *.toc

distclean: clean
	rm -f *.pdf