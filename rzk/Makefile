syntax: src/Language/Rzk/Syntax/Test
pygments: pygments/setup.py
pdf: doc/Syntax.pdf

all: syntax pygments pdf

clean:
	cd src && (make --makefile=Language/Rzk/Makefile clean; cd ../)

src/Language/Rzk/Syntax/Test: src/Language/Rzk/Syntax.cf
	cd src/ \
		&& bnfc -d Language/Rzk/Syntax.cf -p Language.Rzk --makefile=Language/Rzk/Makefile --generic --functor --text-token \
		&& make --makefile=Language/Rzk/Makefile \
		&& rm Language/Rzk/Syntax/Test.hs ; \
		cd ../

pygments/setup.py: src/Language/Rzk/Syntax.cf
	bnfc --pygments src/Language/Rzk/Syntax.cf

doc/Syntax.pdf: doc/Syntax.tex
	latexmk -output-directory=doc/ -pdflatex doc/Syntax.tex

doc/Syntax.tex: src/Language/Rzk/Syntax.cf
	bnfc --latex src/Language/Rzk/Syntax.cf -o doc/

.PHONY: syntax clean all pygments pdf
