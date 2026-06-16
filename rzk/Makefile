# The parser and lexer are generated from the grammar (the source of truth) and
# the results are committed under src/Language/Rzk/Syntax. The package builds
# with build-type: Simple, so regeneration is out of band: run `make
# regen-parser` (from the rzk/ directory) after editing grammar/Syntax.cf, then
# commit the result. The `Parser up-to-date` CI workflow regenerates from the
# grammar and fails if the committed files drift, so this must stay in sync.
#
# Tool versions matter: different BNFC/happy versions emit different output
# (e.g. a version string in the header), which the drift guard sees as a diff.
# The guard pins BNFC 2.9.6.3 and happy 2.2; regenerate with those locally
# (e.g. `cabal install BNFC-2.9.6.3 happy-2.2 alex`).

GRAMMAR    = grammar/Syntax.cf
SYNTAX_DIR = src/Language/Rzk/Syntax

# Canonical regeneration. The first three commands are exactly what the old
# custom Setup.hs ran on every build; they reproduce the committed file set.
# BNFC writes Lex.x/Par.y, which alex/happy turn into the committed Lex.hs and
# Par.hs. We then delete Lex.x/Par.y: leaving them in src/ would make a
# build-type: Simple build try to run alex/happy again (and fail without them),
# which is the whole problem this layout avoids. Only the committed .hs build.
regen-parser:
	bnfc -d -p Language.Rzk --generic --functor --text-token -o src/ $(GRAMMAR)
	alex --ghc $(SYNTAX_DIR)/Lex.x
	happy --array --info --ghc --coerce $(SYNTAX_DIR)/Par.y
	rm -f $(SYNTAX_DIR)/Lex.x $(SYNTAX_DIR)/Par.y

# BNFC-derived auxiliary outputs (not needed for the build).
pygments: $(GRAMMAR)
	bnfc --pygments $(GRAMMAR)

pdf: doc/Syntax.pdf

doc/Syntax.pdf: doc/Syntax.tex
	latexmk -output-directory=doc/ -pdflatex doc/Syntax.tex

doc/Syntax.tex: $(GRAMMAR)
	bnfc --latex $(GRAMMAR) -o doc/

.PHONY: regen-parser pygments pdf
