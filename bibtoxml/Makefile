ROOTDIR := $(PWD)

SRCDIR    := $(ROOTDIR)/src
MAIN      := $(SRCDIR)/Main.hs
BUILDDIR  := $(ROOTDIR)/build
ODIR      := $(BUILDDIR)
OUTPUTDIR := $(BUILDDIR)
BINDIR    := $(ROOTDIR)/bin
BINMAIN   := $(BINDIR)/bibtoxml

.PHONY: build install-dependencies

install-dependencies:
	cabal update && \
	cabal install xml MissingH

build: $(MAIN)
	cd $(SRCDIR) && \
	ghc --make -outputdir $(OUTPUTDIR) -odir $(ODIR) -o $(BINMAIN) $(MAIN)
