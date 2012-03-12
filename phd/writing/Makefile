clean:
	rm *.log *.aux *.dvi *.toc *.bbl *.blg

writeup: candid.bib
	latex writeup.tex
	bibtex writeup
	latex writeup.tex
	latex writeup.tex

candid_up: candid.annot candid.bib
	./scrape.py candid
	./make_annot.py candid
	latex candid.tex
	bibtex candid
	latex candid.tex
	latex candid.tex

candid: candid.annot candid.bib
	./make_annot.py candid
	latex candid.tex
	bibtex candid
	latex candid.tex
	latex candid.tex