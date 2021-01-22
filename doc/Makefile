# == The PDF documentation =====================================================

TEXDIR    := tex
TEXOUTDIR := $(TEXDIR)/out
TEXMAIN   := bibtex-xml-dokumentation
PDFDIR    := pdf

$(TEXOUTDIR):
	mkdir $(TEXOUTDIR)

$(PDFDIR):
	mkdir $(PDFDIR)

pdfdoc: $(TEXDIR)/$(TEXMAIN).tex \
        $(TEXOUTDIR) \
        $(PDFDIR)
	lualatex --interaction=nonstopmode --output-directory=$(TEXOUTDIR) \
	    $(TEXDIR)/$(TEXMAIN).tex > /dev/null
	biber --onlylog $(TEXOUTDIR)/$(TEXMAIN)
	lualatex --interaction=nonstopmode --output-directory=$(TEXOUTDIR) \
	    $(TEXDIR)/$(TEXMAIN).tex > /dev/null
	lualatex --interaction=nonstopmode --output-directory=$(TEXOUTDIR) \
	    $(TEXDIR)/$(TEXMAIN).tex > /dev/null
	mv $(TEXOUTDIR)/$(TEXMAIN).pdf $(PDFDIR)/


# == The project submission folder =============================================

BIBTEXXMLDTD   := ../BibTeX-XML.dtd
BIBTEXDATABASE := resources/Projekt_BIB_original.txt

submission-folder:
	mkdir submission-folder

submission: submission-folder \
            pdfdoc \
            $(BIBTEXXMLDTD) \
            $(BIBTEXDATABASE)
	cp pdf/$(TEXMAIN).pdf submission-folder/
	cp $(BIBTEXXMLDTD)    submission-folder/
	cp $(BIBTEXDATABASE)  submission-folder/
