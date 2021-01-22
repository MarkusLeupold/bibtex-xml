TEXDIR    := tex
TEXOUTDIR := $(TEXDIR)/out
TEXMAIN   := bibtex-xml-dokumentation

pdfdoc: tex/bibtex-xml-dokumentation.tex
	lualatex --interaction=nonstopmode --output-directory=$(TEXOUTDIR) \
	    $(TEXDIR)/$(TEXMAIN).tex > /dev/null
	biber $(TEXOUTDIR)/$(TEXMAIN)
	lualatex --interaction=nonstopmode --output-directory=$(TEXOUTDIR) \
	    $(TEXDIR)/$(TEXMAIN).tex > /dev/null
	lualatex --interaction=nonstopmode --output-directory=$(TEXOUTDIR) \
	    $(TEXDIR)/$(TEXMAIN).tex > /dev/null
	mv $(TEXOUTDIR)/$(TEXMAIN).pdf pdf/
