ROOTDIR        := $(PWD)
TASKDIR        := task
BIBTEXDATABASE := $(TASKDIR)/Projekt_BIB_original.txt
BIBTEXXMLDTD   := BibTeX-XML.dtd

# The bibtoxml program
BIBTOXMLROOTDIR := bibtoxml
BIBTOXML := $(BIBTOXMLROOTDIR)/bin/bibtoxml
$(BIBTOXML):
	cd $(BIBTOXMLROOTDIR); \
	make build; \
	cd $(PWD)


# == The project submission folder =============================================

SUBMISSIONDIR := submission-folder
BIBTEXXMLDOCUMENT := $(SUBMISSIONDIR)/Projekt_BIB_original.xml

$(SUBMISSIONDIR):
	mkdir $(SUBMISSIONDIR)

submission: $(SUBMISSIONDIR) \
            $(BIBTEXDATABASE) \
            $(BIBTOXML) \
            $(BIBTEXXMLDTD)
	cd doc; make build; cd $(ROOTDIR)
	cp doc/pdf/*.pdf $(SUBMISSIONDIR)/
	cp $(BIBTEXDATABASE) $(SUBMISSIONDIR)/
	$(BIBTOXML) -o $(BIBTEXXMLDOCUMENT) $(BIBTEXDATABASE)
	cp $(BIBTEXXMLDTD) $(SUBMISSIONDIR)/


# == Cleanup ===================================================================

clean:
	rm -frd $(SUBMISSIONDIR)
