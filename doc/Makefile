RESOURCES := resources
BIBTEXDATABASE := $(RESOURCES)/Projekt_BIB_original.txt

# Resources that are generated during the documentation build process are put
# into a special folder. This folder should be ignored by Git.
RESOURCESGENERATED := resources-generated
$(RESOURCESGENERATED):
	mkdir $(RESOURCESGENERATED)

BIBTEXXMLDOCUMENT := $(RESOURCESGENERATED)/Projekt_BIB_original.xml

# The bibtoxml program
BIBTOXMLROOTDIR := ../bibtoxml
BIBTOXML := $(BIBTOXMLROOTDIR)/bin/bibtoxml
$(BIBTOXML):
	cd $(BIBTOXMLROOTDIR); \
	make build; \
	cd $(PWD)


# == The BibTeX-XML document ===================================================

$(BIBTEXXMLDOCUMENT): $(BIBTEXDATABASE) $(RESOURCESGENERATED) $(BIBTOXML)
	$(BIBTOXML) -o $(BIBTEXXMLDOCUMENT) $(BIBTEXDATABASE)


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
        $(RESOURCES) \
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
SUBMISSIONDIR  := submission-folder

$(SUBMISSIONDIR):
	mkdir $(SUBMISSIONDIR)

submission: $(SUBMISSIONDIR) \
            pdfdoc \
            $(BIBTEXDATABASE) \
            $(BIBTEXXMLDOCUMENT) \
            $(BIBTEXXMLDTD)
	cp pdf/$(TEXMAIN).pdf   $(SUBMISSIONDIR)/
	cp $(BIBTEXDATABASE)    $(SUBMISSIONDIR)/
	cp $(BIBTEXXMLDOCUMENT) $(SUBMISSIONDIR)/
	cp $(BIBTEXXMLDTD)      $(SUBMISSIONDIR)/


# == Cleanup ===================================================================

clean-everything:
	rm -frd $(SUBMISSIONDIR)
	rm -frd $(PDFDIR)
	rm -frd $(TEXOUTDIR)
	rm -frd $(RESOURCESGENERATED)
