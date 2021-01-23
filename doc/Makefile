RESOURCES := resources

# == The PDF documentation =====================================================

TEXDIR    := tex
TEXOUTDIR := $(TEXDIR)/out
TEXMAIN   := bibtex-xml-dokumentation
TEXOUTPDF := $(TEXOUTDIR)/$(TEXMAIN).pdf
PDFDIR    := pdf
PDFFILE   := $(PDFDIR)/$(TEXMAIN).pdf

$(TEXOUTDIR):
	mkdir $(TEXOUTDIR)

$(PDFDIR):
	mkdir $(PDFDIR)

$(TEXOUTPDF): $(TEXDIR)/$(TEXMAIN).tex \
              $(RESOURCES) \
              $(TEXOUTDIR)
	lualatex --interaction=nonstopmode --output-directory=$(TEXOUTDIR) \
	    $(TEXDIR)/$(TEXMAIN).tex > /dev/null
	biber --onlylog $(TEXOUTDIR)/$(TEXMAIN)
	lualatex --interaction=nonstopmode --output-directory=$(TEXOUTDIR) \
	    $(TEXDIR)/$(TEXMAIN).tex > /dev/null
	lualatex --interaction=nonstopmode --output-directory=$(TEXOUTDIR) \
	    $(TEXDIR)/$(TEXMAIN).tex > /dev/null

$(PDFFILE): $(PDFDIR) $(TEXOUTPDF)
	cp $(TEXOUTDIR)/$(TEXMAIN).pdf $(PDFDIR)/

build: $(PDFFILE)

clean-build: clean build


# == Cleanup ===================================================================

clean:
	rm -frd $(PDFDIR)
	rm -frd $(TEXOUTDIR)
