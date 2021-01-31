ROOTDIR            := $(PWD)
TASKDIR            := task
BIBTEXDATABASE     := $(TASKDIR)/Projekt_BIB_original.txt
BIBTEXXMLDTD       := BibTeX-XML.dtd
BIBTEXXMLXSD       := BibTeX-XML.xsd
BIBTEXXMLTOHTMLXSL := BibTeX-XML-to-html.xsl
STYLECSS           := style.css

# The bibtoxml program
BIBTOXMLROOTDIR := bibtoxml
BIBTOXML := $(BIBTOXMLROOTDIR)/bin/bibtoxml
$(BIBTOXML):
	cd $(BIBTOXMLROOTDIR); \
	make build; \
	cd $(PWD)


# == The project submission folder =============================================

SUBMISSIONDIR := submission-folder
BIBTEXXMLDOCUMENT    := $(SUBMISSIONDIR)/Projekt_BIB_original.xml
BIBTEXXMLDOCUMENTDTD := $(SUBMISSIONDIR)/Projekt_BIB_original_dtd.xml
BIBTEXXMLDOCUMENTXSD := $(SUBMISSIONDIR)/Projekt_BIB_original_xsd.xml
BIBTEXXMLDOCUMENTLOG := $(SUBMISSIONDIR)/Projekt_BIB_original.log
BIBTEXXMLDOCUMENTWITHSTYLE := $(SUBMISSIONDIR)/Projekt_BIB_original_with-style.xml
BIBLIOGRAPHYHTML     := $(SUBMISSIONDIR)/Projekt_BIB_original.html

$(SUBMISSIONDIR):
	mkdir $(SUBMISSIONDIR)

submission: $(SUBMISSIONDIR) \
            $(BIBTEXDATABASE) \
            $(BIBTOXML) \
            $(BIBTEXXMLDTD)
	cd doc; make build; cd $(ROOTDIR)
	cp doc/pdf/*.pdf $(SUBMISSIONDIR)/
	cp $(BIBTEXDATABASE) $(SUBMISSIONDIR)/
	$(BIBTOXML) -o $(BIBTEXXMLDOCUMENT) -l $(BIBTEXXMLDOCUMENTLOG) \
	    $(BIBTEXDATABASE)
	$(BIBTOXML) -o $(BIBTEXXMLDOCUMENTDTD) -l $(BIBTEXXMLDOCUMENTLOG) \
	    --dtd $(BIBTEXXMLDTD) $(BIBTEXDATABASE)
	$(BIBTOXML) -o $(BIBTEXXMLDOCUMENTXSD) -l $(BIBTEXXMLDOCUMENTLOG) \
	    --schema $(BIBTEXXMLXSD) $(BIBTEXDATABASE)
	$(BIBTOXML) -o $(BIBTEXXMLDOCUMENTWITHSTYLE) -l $(BIBTEXXMLDOCUMENTLOG) \
	    --schema $(BIBTEXXMLXSD) \
		--style application/xml $(BIBTEXXMLTOHTMLXSL) \
		$(BIBTEXDATABASE)
	cp $(BIBTEXXMLDTD) $(SUBMISSIONDIR)/
	cp $(BIBTEXXMLXSD) $(SUBMISSIONDIR)/
	cp $(BIBTEXXMLTOHTMLXSL) $(SUBMISSIONDIR)/
	cp $(STYLECSS) $(SUBMISSIONDIR)/
	xsltproc -o $(BIBLIOGRAPHYHTML) $(BIBTEXXMLTOHTMLXSL) \
		$(BIBTEXXMLDOCUMENTXSD)


# == Cleanup ===================================================================

clean:
	rm -frd $(SUBMISSIONDIR)
