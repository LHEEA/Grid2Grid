## Path

DSRC=.

## Document name

DOCNAME=manual

JOB=$(DSRC)/$(DOCNAME)

TEXS=$(wildcard *.tex) $(wildcard *.sty) $(wildcard *.cls)
PICS=$(wildcard *.png) $(filter-out $(JOB).pdf,$(wildcard *.pdf)) $(wildcard *.jpg)
BIBS=$(wildcard *.bib) $(wildcard *.bst)

## Compiler
LATEXFLAGS?=-interaction=nonstopmode -file-line-error
PDFLATEX=pdflatex
BIBTEX=bibtex

## make file

.DEFAULT: all
.PHONY: all clean

all: $(JOB).pdf

# $(JOB).pdf: $(TEXS) $(PICS) $(JOB).aux $(JOB).bbl
# 	@cp -f $(JOB).aux $(JOB).aux.bak
# 	$(PDFLATEX) $(LATEXFLAGS) $(JOB)
# 	@if cmp -s $(JOB).aux $(JOB).aux.bak; \
# 	then touch -r $(JOB).aux.bak $(JOB).aux; \
# 	else NEWS="$$NEWS -W $(JOB).aux"; fi; rm $(JOB).aux.bak; \
# 	if [ -n "$$NEWS" ]; then $(MAKE) $$NEWS $@; fi

$(JOB).pdf: $(TEXS) $(PICS) $(JOB).aux $(JOB).bbl
	$(PDFLATEX) $(LATEXFLAGS) $(JOB)
	if [ -n "$$NEWS" ]; then $(MAKE) $$NEWS $@; fi

$(JOB).aux: | $(TEXS) $(PICS)
	$(PDFLATEX) $(LATEXFLAGS) $(JOB)

$(JOB).bbl: $(JOB).aux $(BIBS)
	$(BIBTEX) $(JOB)

clean:
	rm -f $(JOB).aux $(JOB).log $(JOB).blg $(JOB).bbl $(JOB).out $(JOB).pdf $(JOB).toc
